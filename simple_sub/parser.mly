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
%type <toplevel list> program
%%

program:
  | EOF                                        { [] }
  | LET IDENT EQUAL Term; rest = program       { {is_rec=false; name=$2; body=$4}::rest }
  | LET REC IDENT EQUAL Term; rest = program   { {is_rec=true; name=$3; body=$5}::rest }
 
AtomicTerm:
  | LPAREN Term RPAREN
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

Term:
  | AppTerm { $1 }
  | FN IDENT ARROW Term { Abs($2, $4) }
  | LET IDENT EQUAL Term IN Term { Let{is_rec=false; name=$2; rhs=$4; body=$6} }
  | LET REC IDENT EQUAL Term IN Term { Let{is_rec=true; name=$3; rhs=$5; body=$7} }
  | IF Term THEN Term ELSE Term { App(App(App(Var "if", $2), $4), $6) }
;

RcdList:
  | { [] }
  | IDENT COLON Term { [($1, $3)] }
  | IDENT COLON Term COMMA; rest = RcdList { ($1, $3)::rest }
;
