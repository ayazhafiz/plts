(* Taken from https://github.com/OhadRau/RelevantPromises/blob/master/src/lexer.mll *)

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
let newline = '\r' | '\n' | "\r\n"

let nat = ['0'-'9']+

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule lineComment = parse
  | newline
    { next_line lexbuf; read lexbuf }
  | _
    { lineComment lexbuf }

and read = parse
  | whitespace { read lexbuf }
  | newline    { next_line lexbuf; read lexbuf }

  | nat as b  { INT  (int_of_string b) }

  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "fn"      { FN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }

  | id as id  { IDENT id }

  | "->"      { ARROW }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '{'       { LCURLY }
  | '}'       { RCURLY }
  | ','       { COMMA }
  | ':'       { COLON }
  | '.'       { DOT }
  | '='       { EQUAL }

  | "//"      { lineComment lexbuf }
  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }
