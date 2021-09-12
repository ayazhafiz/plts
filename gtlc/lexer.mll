{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }
}

let whitespace = [' ' '\t']+
let newline = '\n' | "\r\n"

let nat = ['0'-'9']+

let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | whitespace    { read lexbuf }
  | newline       { next_line lexbuf; read lexbuf }

  | "nat"      { TNAT }
  | "bool"     { TBOOL }
  | "\\"       { LAM }
  | "λ"        { LAM }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | ":"        { COLON }
  | "."        { DOT }
  | "let"      { LET }
  | "in"       { IN }
  | "="        { EQ }
  | "->"       { ARROW }
  | "?"        { QMARK }
  | "#t"       { BOOL true }
  | "#f"       { BOOL false }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }

  | ident as id   { IDENT id }
  | nat as n      { NUM (int_of_string n) }

  | "-"       { comment lexbuf }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }

and comment = parse
  | eof { EOF }
  | newline
    { next_line lexbuf; read lexbuf }
  | _
    { comment lexbuf }
