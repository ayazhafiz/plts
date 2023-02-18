open Sedlexing
open Ast_parser

exception AstError of string

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]
let newline = [%sedlex.regexp? '\n' | "\r\n"]
let int = [%sedlex.regexp? Opt '-', Plus '0' .. '9']

let lower =
  [%sedlex.regexp?
    'a' .. 'z', Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'')]

let upper =
  [%sedlex.regexp?
    'A' .. 'Z', Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'')]

let pos_info_of_position ({ pos_lnum; pos_bol; pos_cnum; _ } : Lexing.position)
    =
  (pos_lnum, pos_cnum - pos_bol + 1)

let make (lexbuf : Sedlexing.lexbuf) tok =
  let start, fin = Sedlexing.lexing_positions lexbuf in
  let start, fin = (pos_info_of_position start, pos_info_of_position fin) in
  tok (start, fin)

let rec read (lexbuf : Sedlexing.lexbuf) =
  match%sedlex lexbuf with
  | whitespace -> read lexbuf
  | newline -> read lexbuf
  | "let" -> make lexbuf (fun i -> LET i)
  | "rec" -> make lexbuf (fun i -> REC i)
  | "in" -> make lexbuf (fun i -> IN i)
  | "if" -> make lexbuf (fun i -> IF i)
  | "then" -> make lexbuf (fun i -> THEN i)
  | "else" -> make lexbuf (fun i -> ELSE i)
  | "spawn" -> make lexbuf (fun i -> SPAWN i)
  | "yield" -> make lexbuf (fun i -> YIELD i)
  | "resume" -> make lexbuf (fun i -> RESUME i)
  | "stat" -> make lexbuf (fun i -> STAT i)
  | "`Pending" -> make lexbuf (fun i -> TAG_PENDING i)
  | "`Done" -> make lexbuf (fun i -> TAG_DONE i)
  | "=" -> make lexbuf (fun i -> EQ i)
  | "->" -> make lexbuf (fun i -> ARROW i)
  | "(" -> make lexbuf (fun i -> LPAREN i)
  | ")" -> make lexbuf (fun i -> RPAREN i)
  | "{" -> make lexbuf (fun i -> LBRACE i)
  | "}" -> make lexbuf (fun i -> RBRACE i)
  | "\\" -> make lexbuf (fun i -> LAMBDA i)
  | "true" -> make lexbuf (fun i -> LITBOOL (i, true))
  | "false" -> make lexbuf (fun i -> LITBOOL (i, false))
  | "<" -> make lexbuf (fun i -> LT i)
  | "==" -> make lexbuf (fun i -> EQUALS i)
  | "+" -> make lexbuf (fun i -> PLUS i)
  | "-" -> make lexbuf (fun i -> MINUS i)
  | "*" -> make lexbuf (fun i -> MUL i)
  | "," -> make lexbuf (fun i -> COMMA i)
  | ";" -> make lexbuf (fun i -> SEMI i)
  | "." -> make lexbuf (fun i -> DOT i)
  | "|" -> make lexbuf (fun i -> PIPE i)
  | int ->
      make lexbuf (fun i -> LITINT (i, int_of_string @@ Utf8.lexeme lexbuf))
  | lower -> make lexbuf (fun i -> LOWER (i, Utf8.lexeme lexbuf))
  | "#" -> comment lexbuf
  | eof -> EOF
  | _ ->
      raise
      @@ AstError
           (Printf.sprintf "Unexpected char or sequence: %S"
              (Sedlexing.next lexbuf |> Option.get |> Uchar.to_char
             |> String.make 1))

and comment lexbuf =
  match%sedlex lexbuf with
  | eof -> EOF
  | newline -> read lexbuf
  | any -> comment lexbuf
  | _ -> failwith ""

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
