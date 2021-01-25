open Language
open Lexing
open Typecheck
open Simplify
open Print

(* Top-level typechecking drivers *)

(** [pipeline_typecheck ctx program] infers the types for a program in a
    context, pushes the inferred typings through the simplification pipeline,
    and returns a string of new bindings in the program as well as the context
    updated with those bindings. *)
let pipeline_typecheck ctx program =
  let bindings, ctx = typeProgram ctx program in
  let bindings =
    List.map
      (fun r ->
        Result.bind r (fun (name, ty) ->
            try
              let ty =
                ( match ty with
                | `S simple -> simple
                | `P poly -> instantiate 0 poly )
                |> canonicalizeSimpleTy |> simplifyTy |> coalesceCompactTy
              in
              Ok (name, ty)
            with Failure m -> Error m))
      bindings
  in
  (bindings, ctx)

let default_ctx =
  let b = STyPrim "bool" in
  let i = STyPrim "int" in
  [
    ("true", `S b);
    ("false", `S b);
    ("not", `S (STyFn (b, b)));
    ("succ", `S (STyFn (i, i)));
    ("add", `S (STyFn (i, STyFn (i, i))));
    ( "if",
      let v = STyVar (freshVar 1) in
      `P (PolyTy (0, STyFn (b, STyFn (v, STyFn (v, v))))) );
  ]
  |> List.to_seq |> Ctx.of_seq

(* Repl + File reader *)

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  let result =
    try Some (Parser.program Lexer.read lexbuf) with
    | Lexer.SyntaxError msg ->
        Printf.eprintf "Syntax error: %s at %s" msg (string_of_position lexbuf);
        None
    | _ ->
        Printf.eprintf "Parse error at %s" (string_of_position lexbuf);
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

let pp_results results =
  results
  |> List.map (function
       | Ok (name, ty) -> Printf.sprintf "%s: %s" name (string_of_ty ty)
       | Error msg -> Printf.sprintf "Type error: %s" msg)
  |> String.concat "\n"

let process_program ctx toplevels =
  let bindings, ctx = pipeline_typecheck ctx toplevels in
  let results = pp_results bindings in
  Printf.printf "%s" results;
  ctx

let rec repl ctx =
  Printf.printf "> ";
  flush_all ();
  let buf = ref [] in
  readin buf;
  let input = String.concat "\n" !buf in
  let parsed = parse (Lexing.from_string ~with_positions:true input) in
  let ctx = Option.fold ~none:ctx ~some:(process_program ctx) parsed in
  flush_all ();
  Printf.printf "\n\n";
  flush_all ();
  repl ctx

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

let process_file inFile ctx =
  let fi = openfile inFile in
  let parsed = parse (Lexing.from_channel ~with_positions:true fi) in
  let ctx = Option.fold ~none:ctx ~some:(process_program ctx) parsed in
  flush_all ();
  ctx

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
      let _ = process_file inFile default_ctx in
      ()
  | None -> repl default_ctx

let () = main ()
