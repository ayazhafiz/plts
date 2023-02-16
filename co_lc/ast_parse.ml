open Ast

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let fresh_var_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    ref (Unbd !n)

let fresh_resume_name_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    "Resume_" ^ string_of_int !n

let fresh_parse_ctx () : Ast.parse_ctx =
  { fresh_var = fresh_var_generator (); symbols = Symbol.make () }

let parse s =
  let lexbuf = Ast_lex.from_string s in
  let lex = Ast_lex.provider lexbuf in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Ast_parser.toplevel
  in
  let parse_ctx = fresh_parse_ctx () in
  try
    let parsed = parse lex parse_ctx in
    Ok (parse_ctx.fresh_var, parse_ctx.symbols, parsed)
  with
  | Ast_lex.AstError what ->
      Error
        (Printf.sprintf "Ast error: %s at %s" what
           (string_of_position (Ast_lex.position lexbuf)))
  | Ast_parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position (Ast_lex.position lexbuf)))
