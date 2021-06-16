%{
  open Language
  open Language.Ast
%}

%token <int>    NUM
%token <string> IDENT

%token INT
%token ANY
%token IF
%token IN
%token IS
%token THEN
%token ELSE
%token COMMA
%token CO
%token LPAREN
%token RPAREN
%token EQ
%token AND
%token OR
%token NOT
%token EOF

%left OR
%left AND
%nonassoc NOT

%start toplevel_term
%type <term> toplevel_term
%start toplevel_ty
%type <ty> toplevel_ty
%%

toplevel_term:
  | term EOF { $1 }
toplevel_ty:
  | ty   EOF { $1 }

term:
  | NUM         { Num $1 }
  | IDENT       { Var $1 }
  | LPAREN term COMMA tuple_list RPAREN { Tup ($2::$4) }
  | IDENT term  { App ($1, $2) }
  | IDENT LPAREN param_list RPAREN EQ term IN term { Dec ($1, $3, $6, $8) }
  | IF IDENT IS ty THEN term ELSE term { If ($2, $4, $6, $8) }
tuple_list:
  | term { [$1] }
  | term COMMA tuple_list { $1::$3 }
param_list:
  | { [] }
  | IDENT CO ty { [($1, $3)] }
  | IDENT CO ty COMMA param_list { ($1, $3)::$5 }

ty:
  | INT          { Int }
  | ANY          { Any }
  | LPAREN ty COMMA tuple_ty_list RPAREN { Tuple ($2::$4) }
  | NOT ty       { Not $2 }
  | ty AND ty    { Inter (TySet.of_list [$1;$3]) }
  | ty OR ty     { Union (TySet.of_list [$1;$3]) }
  | LPAREN ty RPAREN { $2 }
tuple_ty_list:
  | ty { [$1] }
  | ty COMMA tuple_ty_list { $1::$3 }
