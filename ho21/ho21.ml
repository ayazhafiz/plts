open Duotyping
open Print

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let parse_queries s =
  let lexbuf = Lexing.from_string ~with_positions:true s in
  try
    let parsed = Parser.queries Lexer.read lexbuf in
    Ok parsed
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position lexbuf.lex_curr_p))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position lexbuf.lex_curr_p))

let string_of_query prettify_symbols (t1, t2) =
  string_of_query prettify_symbols t1 t2

let judge_query (t1, t2) = judge t1 t2

let string_of_judgement with_debug_data prettify_symbols =
  string_of_judgement with_debug_data prettify_symbols
