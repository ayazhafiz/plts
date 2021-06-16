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

  | nat as b      { NUM (int_of_string b) }

  | "int"         { INT }
  | "any"         { ANY }
  | "if"          { IF }
  | "in"          { IN }
  | "is"          { IS }
  | "then"        { THEN }
  | "else"        { ELSE }

  | ident as id   { IDENT id }

  | ","       { COMMA }
  | ":"       { CO }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "="       { EQ }
  | "&"       { AND }
  | "|"       { OR }
  | "!"       { NOT }

  | "#"      { comment lexbuf }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }

and comment = parse
  | eof { raise (SyntaxError "Unterminated comment") }
  | newline
    { next_line lexbuf; read lexbuf }
  | _
    { comment lexbuf }
