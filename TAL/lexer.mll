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

let macro = ['$'] ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let tymacro = ['%'] ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | whitespace    { read lexbuf }
  | newline       { next_line lexbuf; read lexbuf }

  | "fix"     { FIX }
  | "int"     { INT }
  | "if0"     { IF0 }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "let"     { LET }
  | "in"      { IN }
  | "="       { EQ }
  | "->"      { ARROW }
  | "."       { DOT }
  | ","       { COMMA }
  | ":"       { COLON }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "<"       { LANGLE }
  | ">"       { RANGLE }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "Λ"       { BIGLAM }
  | "∀"       { FORALL }

  | ident as id   { IDENT id }
  | macro as id   { MACRO id }
  | tymacro as id   { TYMACRO id }
  | nat as n      { NUM (int_of_string n) }

  | "#"      { comment lexbuf }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }

and comment = parse
  | eof { EOF }
  | newline
    { next_line lexbuf; read lexbuf }
  | _
    { comment lexbuf }
