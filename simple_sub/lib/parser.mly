%{
 open Language
%}

%token <int>    INT
%token <string> IDENT

%token LET REC IN FN ARROW EQUAL
%token IF THEN ELSE
%token LPAREN RPAREN
%token LCURLY RCURLY
%token COMMA COLON DOT

%token EOF

%start program
%type <Language.toplevel list> program
%start term
%type <Language.term> term
%%

program:
  | EOF                                        { [] }
  | LET IDENT EQUAL term; rest = program       { {is_rec=false; name=$2; body=$4}::rest }
  | LET REC IDENT EQUAL term; rest = program   { {is_rec=true; name=$3; body=$5}::rest }
 
AtomicTerm:
  | LPAREN term RPAREN
           { $2 }
  | INT    { Num $1 }
  | IDENT  { Var $1 }
  | LCURLY RcdList RCURLY
           { Record($2) }
;

PathTerm: /* terms that have a "path", like a projection. */
  | AtomicTerm { $1 }
  | PathTerm DOT IDENT
           { RecordProject($1, $3) }
;

AppTerm: /* terms that are applications. */
  | PathTerm { $1 }
  | AppTerm PathTerm { App($1, $2) }

term:
  | AppTerm { $1 }
  | FN IDENT ARROW term { Abs($2, $4) }
  | LET IDENT EQUAL term IN term { Let{is_rec=false; name=$2; rhs=$4; body=$6} }
  | LET REC IDENT EQUAL term IN term { Let{is_rec=true; name=$3; rhs=$5; body=$7} }
  | IF term THEN term ELSE term { App(App(App(Var "if", $2), $4), $6) }
;

RcdList:
  | { [] }
  | IDENT COLON term { [($1, $3)] }
  | IDENT COLON term COMMA; rest = RcdList { ($1, $3)::rest }
;
