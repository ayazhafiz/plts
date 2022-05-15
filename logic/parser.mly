%{
  open Syntax
%}

%token <string> IDENT

%token TOP
%token BOT
%token LPAREN
%token RPAREN
%token NEG
%token AND
%token OR
%token ARROW
%token EOF

%left ARROW
%left OR
%left AND
%nonassoc NEG

%start formula_entry
%type <formula> formula_entry
%%

formula_entry:
  | form EOF { $1 }

form:
  | IDENT         { Var $1 }
  | TOP           { Top }
  | BOT           { Bot }
  | NEG form      { Neg($2) }
  | form OR form  { Disj($1, $3) }
  | form AND form { Conj($1, $3) }
  | form ARROW form { Imp($1, $3) }
  | LPAREN form RPAREN { $2 }
