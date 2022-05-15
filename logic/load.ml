let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let parse_safe s =
  let lexbuf = Lexing.from_string ~with_positions:true s in
  try
    let parsed = Parser.formula_entry Lexer.read lexbuf in
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

let parse s = Result.get_ok (parse_safe s)
