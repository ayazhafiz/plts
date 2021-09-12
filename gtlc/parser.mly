%{
  open Language
%}

%token <string> IDENT
%token <int>    NUM
%token <bool>   BOOL

%token NAT
%token LAM
%token LPAREN
%token RPAREN
%token COLON
%token DOT
%token LET
%token IN
%token EQ
%token ARROW
%token QMARK
%token IF
%token THEN
%token ELSE
%token EOF

%start toplevel_expr
%type <unelaborated_expr> toplevel_expr
%%

toplevel_expr:
  | expr EOF { $1 }

expr:
  | app_like_expr { $1 }
  | LAM IDENT DOT expr { Just(Lam($2, TUnknown, $4)) }
  | LAM IDENT COLON ty DOT expr { Just(Lam($2, $4, $6)) }
  | LET IDENT EQ expr IN expr { Just(App(Just(Lam($2, TUnknown, $6)), $4)) }
  | LET IDENT COLON ty EQ expr IN expr { Just(App(Just(Lam($2, $4, $8)), $6)) }
  | IF expr THEN expr ELSE expr { Just(If($2, $4, $6)) }

app_like_expr:
  | atomic_expr { $1 }
  | app_like_expr atomic_expr { Just(App($1, $2)) }

atomic_expr:
  | LPAREN expr RPAREN { $2 }
  | IDENT { Just(Var $1) }
  | NUM { Just(Nat $1) }
  | BOOL { Just(Bool $1) }

ty:
  | arrow_like_type { $1 }

arrow_like_type:
  | atomic_type { $1 }
  | atomic_type ARROW arrow_like_type { TArrow($1, $3) }

atomic_type:
  | LPAREN ty RPAREN { $2 }
  | NAT   { TNat }
  | BOOL  { TBool }
  | QMARK { TUnknown }
