open Language
open Typecheck
open Simplify

(*
let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  let result =
    try Some (Parser.toplevel Lexer.read lexbuf) with
    | Lexer.SyntaxError msg ->
        Printf.eprintf "Syntax error: %s at %s\n" msg
          (string_of_position lexbuf);
        None
    | _ ->
        Printf.eprintf "Parse error at %s\n" (string_of_position lexbuf);
        None
  in
  Parsing.clear_parser ();
  result

let ends_with s1 s2 =
  let n1 = String.length s1 and n2 = String.length s2 in
  n1 >= n2 && String.sub s1 (n1 - n2) n2 = s2

let readin buf =
  try
    while true do
      let line = read_line () in
      if ends_with line ";;" then (
        let trim = String.sub line 0 (String.length line - 2) in
        buf := !buf @ [ trim ];
        raise End_of_file )
      else buf := !buf @ [ line ]
    done
  with End_of_file -> Printf.printf "\n"

type mode = Eval | GenC | GenCAddRt

let parseMode m =
  match m with
  | "eval" -> Eval
  | "codegenC" -> GenC
  | "codegenC-rt" -> GenCAddRt
  | _ ->
      failwith
        (Printf.sprintf
           "Unknown mode %s; must be 'eval', 'codegenC', or 'codegenC-rt' \
            (-rt=add runtime)"
           m)

let pr_binding what ty = Printf.sprintf "%s :: %s" what ty

let handle_mode mode ctx fns expr exprTy to_print =
  match mode with
  | Eval ->
      let evaled = eval ctx expr in
      let exprBind = pr_binding (string_of_expr evaled) (string_of_ty exprTy) in
      to_print @ [ exprBind ]
  | GenC -> [ codegen_c fns expr ]
  | GenCAddRt -> [ codegen_c_w_rt fns expr ]

let process_fn (ctx, to_print) (Fn (name, _, _, _) as fn) =
  let ty = typecheck_fn ctx fn in
  let bind = pr_binding name (string_of_ty ty) in
  (Ctx.add_fn fn ty ctx, to_print @ [ bind ])

let process_expr_opt mode (ctx, to_print) fns expr =
  match expr with
  | None -> (ctx, to_print)
  | Some expr ->
      let ty = typecheck ctx expr in
      let to_print = handle_mode mode ctx fns expr ty to_print in
      (ctx, to_print)

let process_program (mode, ctx) { fns; expr } =
  let ctx, to_print = List.fold_left process_fn (ctx, []) fns in
  let ctx, to_print = process_expr_opt mode (ctx, to_print) fns expr in
  Printf.printf "%s" (String.concat "\n" to_print);
  (mode, ctx)

let rec repl ((_, ctx) as oldEnv) =
  Printf.printf "> ";
  flush_all ();
  let buf = ref [] in
  readin buf;
  let input = String.concat "\n" !buf in
  let parsed = parse (Lexing.from_string ~with_positions:true input) in
  let newEnv =
    try
      match parsed with
      | None -> oldEnv
      | Some (Mode m) ->
          let mode = parseMode m in
          Printf.printf "Set mode \"%s\"" m;
          (mode, ctx)
      | Some (Program p) -> process_program oldEnv p
    with Failure msg ->
      Printf.eprintf "%s" msg;
      oldEnv
  in
  flush_all ();
  Printf.printf "\n\n";
  flush_all ();
  repl newEnv

let searchpath = ref [ "" ]

let openfile inFile =
  let rec trynext l =
    match l with
    | [] -> failwith ("Could not find " ^ inFile)
    | d :: rest -> (
        let name = if d = "" then inFile else d ^ "/" ^ inFile in
        try open_in name with Sys_error _ -> trynext rest )
  in
  trynext !searchpath

let process_file inFile env =
  let fi = openfile inFile in
  let lexbuf = Lexing.from_channel ~with_positions:true fi in
  let newEnv =
    match parse lexbuf with
    | None -> failwith "No emit"
    | Some (Mode _) ->
        failwith
          "Cannot set mode for single file; use command line flags or repl"
    | Some (Program p) -> process_program env p
  in
  flush_all ();
  newEnv

let mode = ref Eval

let argDefs =
  [
    ( "-I",
      Arg.String (fun f -> searchpath := f :: !searchpath),
      "Append a directory to the search path" );
    ( "-M",
      Arg.String
        (fun m -> try mode := parseMode m with Failure m -> raise (Arg.Bad m)),
      "Mode by which program should be processed; either 'eval', 'codegenC', \
       or 'codegenC-rt'. Default: 'eval'" );
  ]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
    (fun s ->
      match !inFile with
      | Some _ -> raise (Arg.Bad "You must specify at most one input file")
      | None -> inFile := Some s)
    "";
  !inFile

let main () =
  match parseArgs () with
  | Some inFile ->
      let _ = process_file inFile (!mode, Ctx.empty) in
      ()
  | None -> repl (!mode, Ctx.empty)

*)

let () = ()
