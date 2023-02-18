include Surface
open Util

type file = NoFile | Base of string

let make_file_from ext = function
  | NoFile -> Filename.temp_file "asti" ("." ^ ext)
  | Base s -> s ^ "." ^ ext

(* Driver *)
type phase = Parse | Solve | Ir | Eval
type emit = Print | Elab

let assoc_flip l = List.map (fun (a, b) -> (b, a)) l

let phase_list =
  [ (Parse, "parse"); (Solve, "solve"); (Ir, "ir"); (Eval, "eval") ]

let emit_list = [ (Print, "print"); (Elab, "elab") ]
let phase_of_string s = List.assoc_opt s @@ assoc_flip phase_list
let string_of_phase p = List.assoc p phase_list
let emit_of_string s = List.assoc_opt s @@ assoc_flip emit_list
let string_of_emit e = List.assoc e emit_list
let unlines = String.concat "\n"

let phases =
  List.map string_of_phase @@ List.sort_uniq compare @@ List.map fst phase_list

let emits =
  List.map string_of_emit @@ List.sort_uniq compare @@ List.map fst emit_list

type command = phase * emit
type command_err = string * [ `InvalidPhase | `InvalidEmit | `Unparseable ]

let string_of_command_err (s, e) =
  s ^ ": "
  ^
  match e with
  | `InvalidPhase -> "invalid phase"
  | `InvalidEmit -> "invalid emit"
  | `Unparseable -> "cannot parse this command"

type raw_program = string list

let raw_program_of_string = String.split_on_char '\n'
let raw_program_of_lines = Fun.id

type queries = loc list
type program = string list * queries

type preprocessed = {
  raw_program : raw_program;
  program : program;
  commands : (command, command_err) result list;
  file : file;
}

let re_cmds = Str.regexp {|# \+\([a-z]+\) -\([a-z\-]+\)|}
let re_query = Str.regexp {|\(\^+\)|}
let starts_command = String.starts_with ~prefix:"# +"
let starts_out = String.starts_with ~prefix:"> "

let parse_command line =
  if Str.string_match re_cmds line 0 then
    let phase = Str.matched_group 1 line in
    let emit = Str.matched_group 2 line in
    match (phase_of_string phase, emit_of_string emit) with
    | Some p, Some e -> Ok (p, e)
    | None, _ -> Error (line, `InvalidPhase)
    | _, None -> Error (line, `InvalidEmit)
  else Error (line, `Unparseable)

let program_without_output =
  let rec go = function
    | [] -> []
    | "" :: line :: _ when starts_out line -> []
    | l :: rest -> l :: go rest
  in
  go

let program_without_commands =
  let rec go = function
    | [] -> []
    | line :: rest -> if starts_command line then go rest else line :: rest
  in
  go

let user_ann_program (lines : raw_program) : string =
  unlines @@ program_without_commands @@ program_without_output lines

let preprocess file (lines : raw_program) : preprocessed =
  let file =
    match file with
    | Some f -> Base (Filename.remove_extension f)
    | None -> NoFile
  in
  (* commands in the header *)
  let commands =
    let rec parse = function
      | [] -> []
      | line :: rest ->
          if starts_command line then parse_command line :: parse rest else []
    in
    parse lines
  in
  (* raw user input including commands and queries but before the output; we
     need this for printing back *)
  let raw_program = program_without_output lines in
  (* parse N queries on a single line *)
  let parse_line_queries lineno line : loc list =
    let rec search start =
      try
        let start = Str.search_forward re_query line start in
        let fin = start + (String.length @@ Str.matched_string line) in
        (* + 1 because positions are 1-indexed *)
        (start + 1, fin + 1) :: search fin
      with Not_found -> []
    in
    let ranges = search 0 in
    List.map (fun (start, fin) -> ((lineno, start), (lineno, fin))) ranges
    |> List.rev
    (* reverse because we want the last query to be processed first, and printed on the first line *)
  in
  (* program ignoring commands and removing query lines *)
  let program_lines, queries =
    let rec parse lineno = function
      | [] -> ([], [])
      | l :: rest when starts_command l -> parse lineno rest
      | l :: _ when starts_out l -> ([], [])
      | line :: rest ->
          let queries = parse_line_queries (lineno - 1) line in
          if List.length queries == 0 then
            (* no queries, include this line *)
            let rest_lines, rest_queries = parse (lineno + 1) rest in
            (line :: rest_lines, rest_queries)
          else
            (* queries - return them and throw away the line *)
            let rest_lines, rest_queries = parse lineno rest in
            (rest_lines, queries @ rest_queries)
    in
    parse 1 lines
  in
  { raw_program; program = (program_lines, queries); commands; file }

type processed_command = command * string

let postprocess (raw_program : raw_program) (commands : processed_command list)
    : string =
  let reflow_out s =
    unlines @@ List.map (fun s -> "> " ^ s) @@ String.split_on_char '\n' s
  in
  let cmd_out =
    List.map
      (fun ((phase, emit), str) ->
        [
          "";
          Printf.sprintf "> +%s -%s" (string_of_phase phase)
            (string_of_emit emit);
          reflow_out str;
        ])
      commands
  in
  String.concat "\n" @@ raw_program @ List.flatten cmd_out

type compile_output = string

let string_of_compile_output = Fun.id

type compile_err =
  | ParseErr of string
  | SolveErr of string
  | ElabErr of [ `NoQueries | `TypeNotFound of loc ]
  | BadEmit of phase * emit
  | NoHover

let string_of_compile_err = function
  | ParseErr s -> "Parse error: " ^ s
  | SolveErr s -> "Solve error: " ^ s
  | ElabErr e -> (
      "Elab error: "
      ^
      match e with
      | `NoQueries -> "no queries given!"
      | `TypeNotFound loc -> "Type not found at " ^ string_of_loc loc)
  | BadEmit (p, e) ->
      "Commit do " ^ string_of_emit e ^ " for phase " ^ string_of_phase p
  | NoHover -> "No hover location found"

type compile_result = (compile_output, compile_err) result

let ( >>= ) = Result.bind

let reflow_lines prefix lines =
  String.split_on_char '\n' lines
  |> List.map (( ^ ) prefix)
  |> String.concat "\n"

let process_one _file (lines, queries) (phase, emit) : compile_result =
  let input = unlines lines in
  let parse s = Result.map_error (fun s -> ParseErr s) @@ Ast_parse.parse s in
  let solve (fresh_var, symbols, e) =
    Result.map_error (fun s -> SolveErr s)
    @@ Ty_solve.infer_program fresh_var symbols e
  in
  let ir (symbols, e) =
    Result.ok @@ (symbols, Vm_optimize.optimize_program @@ Vm_conv.compile e)
  in
  let eval (symbols, ir) = Result.ok @@ (symbols, Vm_interp.interp ir) in
  let elab (symbols, program) =
    if List.length queries = 0 then Error (ElabErr `NoQueries)
    else
      let open Either in
      let queries =
        List.map (fun l -> (l, Service.type_at l program)) queries
      in
      let one_query (((_, cstart), (_, cend)) as loc) =
        let num_caret = cend - cstart in
        let prefix =
          "#"
          (* - 1 because positions are 1-indexed *)
          (* - 1 to make room for the starting `#` *)
          ^ String.init (cstart - 1 - 1) (fun _ -> ' ')
          ^ String.init num_caret (fun _ -> '^')
          ^ " "
        in
        match List.assoc loc queries with
        | None -> Right (ElabErr (`TypeNotFound loc))
        | Some ty ->
            let s_ty = Service.print_type symbols ty in
            Left (reflow_lines prefix s_ty)
      in
      let rec recreate lineno lines =
        let queries =
          List.filter (fun (((l, _), _), _) -> l == lineno) queries
        in
        match (lines, queries) with
        | [], _ -> []
        | l :: rest, [] -> Left l :: recreate (lineno + 1) rest
        | l :: rest, queries ->
            let rest = recreate (lineno + 1) rest in
            let queries = queries |> List.map fst |> List.map one_query in
            Left l :: (queries @ rest)
      in
      let oks, errs = List.partition_map Fun.id @@ recreate 1 lines in
      match errs with e :: _ -> Error e | [] -> Ok (unlines oks)
  in

  let ( &> ) a b = Result.map b a in
  let print_parsed (_tenv, symbols, program) =
    Ast.string_of_program ~width:default_width symbols program
  in
  let print_solved (symbols, program) =
    Ast.string_of_program ~width:default_width symbols program
  in
  let print_ir (symbols, program) =
    Vm_op.string_of_program ~width:default_width symbols program
  in
  let print_evaled (symbols, (values, ty)) =
    Vm_readback.readback symbols values ty
  in
  match (phase, emit) with
  | Parse, Print -> input |> parse &> print_parsed
  | Solve, Print -> input |> parse >>= solve &> print_solved
  | Solve, Elab -> input |> parse >>= solve >>= elab
  | Ir, Print -> input |> parse >>= solve >>= ir &> print_ir
  | Eval, Print -> input |> parse >>= solve >>= ir >>= eval &> print_evaled
  | phase, emit -> Error (BadEmit (phase, emit))

let hover_info lines lineco =
  let parse s = Result.map_error (fun s -> ParseErr s) @@ Ast_parse.parse s in
  let solve (fresh_var, symbols, e) =
    Result.map_error (fun s -> SolveErr s)
    @@ Ty_solve.infer_program fresh_var symbols e
  in
  let hover (symbols, program) =
    Service.hover_info symbols lineco program |> Option.to_result ~none:NoHover
  in
  let hover_info = unlines lines |> parse >>= solve >>= hover in
  hover_info
