{
open Language
open Lexing
open Parser
open CamomileLibrary

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }

let make lexbuf tok =
  let pos = lexbuf.lex_curr_p in
  let len = Lexing.lexeme lexbuf |> UTF8.length in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  let start = (pos.pos_lnum, col - len, lexbuf.lex_curr_pos - len) in
  let fin = (pos.pos_lnum, col, lexbuf.lex_curr_pos) in
  tok { start; fin }
}

let whitespace = [' ' '\t']+
let newline = '\n' | "\r\n"

let nat = ['0'-'9']+

let ident = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | whitespace    { read lexbuf }
  | newline       { next_line lexbuf; read lexbuf }

  | "nat"      { make lexbuf (fun i -> TNAT i) }
  | "bool"     { make lexbuf (fun i -> TBOOL i) }
  | "\\"       { make lexbuf (fun i -> LAM i) }
  | "λ"        { make lexbuf (fun i -> LAM i) }
  | "("        { make lexbuf (fun i -> LPAREN i) }
  | ")"        { make lexbuf (fun i -> RPAREN i) }
  | ":"        { make lexbuf (fun i -> COLON i) }
  | "."        { make lexbuf (fun i -> DOT i) }
  | "let"      { make lexbuf (fun i -> LET i) }
  | "in"       { make lexbuf (fun i -> IN i) }
  | "="        { make lexbuf (fun i -> EQ i) }
  | "->"       { make lexbuf (fun i -> ARROW i) }
  | "?"        { make lexbuf (fun i -> QMARK i) }
  | "_"        { make lexbuf (fun i -> USCORE i) }
  | "#t"       { make lexbuf (fun i -> BOOL (true, i)) }
  | "#f"       { make lexbuf (fun i -> BOOL (false, i)) }
  | "if"       { make lexbuf (fun i -> IF i) }
  | "then"     { make lexbuf (fun i -> THEN i) }
  | "else"     { make lexbuf (fun i -> ELSE i) }
  | "ref"      { make lexbuf (fun i -> REF i) }
  | "!"        { make lexbuf (fun i -> DEREF i) }
  | ":="       { make lexbuf (fun i -> DEFEQ i) }
  | ";"        { make lexbuf (fun i -> SEMI i) }

  | ident as id   { make lexbuf (fun i -> IDENT (id, i)) }
  | nat as n      { make lexbuf (fun i -> NUM (int_of_string n, i)) }

  | "-"       { comment lexbuf }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }

and comment = parse
  | eof { EOF }
  | newline
    { next_line lexbuf; read lexbuf }
  | _
    { comment lexbuf }
