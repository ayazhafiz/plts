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

let boolean = "true" | "false"

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule string_literal strbuf = parse
  | eof
    { EOF }
  | "\\\"" as q
    { Buffer.add_string strbuf q;
      string_literal strbuf lexbuf }
  | '"'
    { STRING (Buffer.contents strbuf |> Scanf.unescaped) }
  | '\n'
    { new_line lexbuf;
      Buffer.add_char strbuf '\n';
      string_literal strbuf lexbuf }
  | _ as c
    { Buffer.add_char strbuf c;
      string_literal strbuf lexbuf }

and read = parse
  | whitespace     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }

  | nat as b      { NAT  (int_of_string b) }
  | '"'
    { string_literal (Buffer.create 100) lexbuf }
  | boolean as b  { BOOL (bool_of_string b) }

  | "mode"    { MODE }

  | "fn"      { FN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }

  (* Type narrowing *)
  | "is"      { IS }
  | "in"      { IN }

  | "unit"    { TYPE_UNIT }
  | "bool"    { TYPE_BOOL }
  | "nat"     { TYPE_NAT }
  | "string"  { TYPE_STRING }

  | id as id  { IDENT id }

  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LEFT_BRACKET }
  | ']'       { RIGHT_BRACKET }
  | '{'       { LCURLY }
  | '}'       { RCURLY }
  | ','       { COMMA }
  | ':'       { COLON }
  | ';'       { SEMI }
  | '|'       { VBAR }
  | '.'       { DOT }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }
