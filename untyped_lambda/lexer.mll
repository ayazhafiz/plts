{
  open Parser
}

let name = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '!' '@' '$' '%' '&' '*' '-' '+' '|' '\\'
            '[' ']' '{' '}' ',' '<' '=' '>' '?' '/' '~' '`' ]+

rule token = parse
    "--" [^'\n']* '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\r' '\t']     { token lexbuf }
  | name                { NAME (Lexing.lexeme lexbuf) }
  | ":context"          { CONTEXT }
  | ":help"             { HELP }
  | ":quit"             { QUIT }
  | ":constant"         { CONSTANT }
  | ":eager"            { EAGER }
  | ":lazy"             { LAZY }
  | ":deep"             { DEEP }
  | ":shallow"          { SHALLOW }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ":="                { COLONEQ }
  | '.'                 { PERIOD }
  | '^'                 { LAMBDA }
  | ';'                 { SEMI }
  | eof                 { EOF }
