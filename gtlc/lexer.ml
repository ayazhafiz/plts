open Language
open Parser
open Sedlexing

exception SyntaxError of string

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]

let newline = [%sedlex.regexp? '\n' | "\r\n"]

let nat = [%sedlex.regexp? Plus '0' .. '9']

let ident =
  [%sedlex.regexp?
    'a' .. 'z', Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'')]

let lam = [%sedlex.regexp? "\u{03BB}"]

let pos_info_of_position ({ pos_lnum; pos_bol; pos_cnum; _ } : Lexing.position)
    =
  (pos_lnum, pos_cnum - pos_bol + 1, pos_cnum)

let make (lexbuf : Sedlexing.lexbuf) tok =
  let start, fin = Sedlexing.lexing_positions lexbuf in
  let start, fin = (pos_info_of_position start, pos_info_of_position fin) in
  tok { start; fin }

let rec read (lexbuf : Sedlexing.lexbuf) =
  match%sedlex lexbuf with
  | whitespace -> read lexbuf
  | newline -> read lexbuf
  | "nat" -> make lexbuf (fun i -> TNAT i)
  | "bool" -> make lexbuf (fun i -> TBOOL i)
  | "\\" -> make lexbuf (fun i -> LAM i)
  | 0x3BB (* λ *) -> make lexbuf (fun i -> LAM i)
  | "(" -> make lexbuf (fun i -> LPAREN i)
  | ")" -> make lexbuf (fun i -> RPAREN i)
  | ":" -> make lexbuf (fun i -> COLON i)
  | "." -> make lexbuf (fun i -> DOT i)
  | "let" -> make lexbuf (fun i -> LET i)
  | "in" -> make lexbuf (fun i -> IN i)
  | "=" -> make lexbuf (fun i -> EQ i)
  | "->" -> make lexbuf (fun i -> ARROW i)
  | "?" -> make lexbuf (fun i -> QMARK i)
  | "_" -> make lexbuf (fun i -> USCORE i)
  | "#t" -> make lexbuf (fun i -> BOOL (true, i))
  | "#f" -> make lexbuf (fun i -> BOOL (false, i))
  | "if" -> make lexbuf (fun i -> IF i)
  | "then" -> make lexbuf (fun i -> THEN i)
  | "else" -> make lexbuf (fun i -> ELSE i)
  | "ref" -> make lexbuf (fun i -> REF i)
  | "!" -> make lexbuf (fun i -> DEREF i)
  | ":=" -> make lexbuf (fun i -> DEFEQ i)
  | ";" -> make lexbuf (fun i -> SEMI i)
  | ident -> make lexbuf (fun i -> IDENT (Utf8.lexeme lexbuf, i))
  | nat -> make lexbuf (fun i -> NUM (int_of_string (Utf8.lexeme lexbuf), i))
  | "-" -> comment lexbuf
  | eof -> EOF
  | _ ->
      raise
      @@ SyntaxError
           (Printf.sprintf "Unexpected char or sequence: %S"
              (Sedlexing.next lexbuf |> Option.get |> Uchar.to_char
             |> String.make 1))

and comment lexbuf =
  match%sedlex lexbuf with
  | eof -> EOF
  | newline -> read lexbuf
  | _ -> comment lexbuf

let from_string s =
  let lexbuf = Utf8.from_string s in
  Sedlexing.set_position lexbuf
    { pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  lexbuf

let provider lexbuf () =
  let tok = read lexbuf in
  let start, fin = Sedlexing.lexing_positions lexbuf in
  (tok, start, fin)

let position lexbuf = Sedlexing.lexing_positions lexbuf |> fst
