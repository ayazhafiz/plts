%{
  open F
%}

%token <string> IDENT
%token <int>    NUM

%token FIX
%token INT
%token IF0
%token THEN
%token ELSE
%token DOT
%token COMMA
%token COLON
%token LPAREN
%token RPAREN
%token LANGLE
%token RANGLE
%token ARROW
%token PLUS
%token MINUS
%token TIMES
%token BIGLAM
%token FORALL
%token EOF

%left PLUS MINUS
%left TIMES

%start toplevel_term
%type <term> toplevel_term
%%

toplevel_term:
  | term EOF { $1 }

term:
  | bigterm { $1 }
  | bigterm COLON ty { Annot($1, $3) }

bigterm:
  | atomic_term LANGLE ty RANGLE { TyApp($1, $3) }
  | atomic_term DOT NUM { Proj($1, $3) }
  | binary_expr { $1 }
  | app_like_term { $1 }
  | FIX IDENT LPAREN IDENT COLON ty RPAREN COLON ty DOT term { Fix {
      name=$2;
      param=$4;
      param_ty=$6;
      ret_ty=$9;
      body=$11 } }
  | BIGLAM IDENT DOT term { TyAbs($2, $4) }
  | IF0 term THEN term ELSE term { If0($2, $4, $6) }

app_like_term:
  | atomic_term { $1 }
  | app_like_term atomic_term { App ($1, $2) }

binary_expr:
  | term PLUS term { Op (Plus, $1, $3) }
  | term MINUS term { Op (Minus, $1, $3) }
  | term TIMES term { Op (Times, $1, $3) }

atomic_term:
  | LPAREN term RPAREN { $2 }
  | LPAREN term COMMA tuple_term_rest { Tup ($2::$4) }
  | IDENT { Var $1 }
  | NUM { Int $1 }

tuple_term_rest:
  | term RPAREN { [$1] }
  | term COMMA tuple_term_rest { $1::$3 }

ty:
  | arrow_like_type { $1 }
  | FORALL IDENT DOT ty { TAll ($2, $4) }

arrow_like_type:
  | atomic_type { $1 }
  | atomic_type ARROW arrow_like_type { TArrow ($1, $3) }

atomic_type:
  | LPAREN ty RPAREN { $2 }
  | LPAREN ty COMMA tuple_type_rest { TTup ($2::$4) }
  | INT { TInt }
  | IDENT { TName $1 }

tuple_type_rest:
  | ty RPAREN { [$1] }
  | ty COMMA tuple_type_rest { $1::$3 }
