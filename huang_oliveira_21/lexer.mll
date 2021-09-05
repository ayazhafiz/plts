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

let ident = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | whitespace    { read lexbuf }
  | newline       { next_line lexbuf; read lexbuf }

  | ident as id   { IDENT id }

  | "*"  | "\u{22A4}" { TOP }
  | "!"  | "\u{22A5}" { BOT }
  | "&"  | "\u{2227}" { AND }
  | "|"  | "\u{2228}" { OR }
  | "->" | "\u{2192}" { ARROW }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "??"      { QUERY }

  | "--"      { comment lexbuf }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }

and comment = parse
  | eof { EOF }
  | newline
    { next_line lexbuf; read lexbuf }
  | _
    { comment lexbuf }
