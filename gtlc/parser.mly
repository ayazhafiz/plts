%{
  open Language
%}

%token <string> IDENT
%token <int>    NUM
%token <bool>   BOOL

%token TNAT
%token TBOOL
%token LAM
%token LPAREN
%token RPAREN
%token COLON
%token DOT
%token USCORE
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
%type <(unit -> ty) -> unelaborated_expr> toplevel_expr
%%

toplevel_expr:
  | expr EOF { fun ft -> $1 ft }

expr:
  | app_like_expr { fun ft -> $1 ft }
  | LAM IDENT DOT expr { fun ft -> Just(Lam($2, TUnknown, $4 ft)) }
  | LAM IDENT COLON ty DOT expr { fun ft -> Just(Lam($2, $4 ft, $6 ft)) }
  | LET IDENT EQ expr IN expr { fun ft ->
      Just(App(Just(Lam($2, TUnknown, $6 ft)), $4 ft))
  }
  | LET IDENT COLON ty EQ expr IN expr { fun ft ->
      Just(App(Just(Lam($2, $4 ft, $8 ft)), $6 ft))
  }
  | IF expr THEN expr ELSE expr { fun ft -> Just(If($2 ft, $4 ft, $6 ft)) }

app_like_expr:
  | atomic_expr { fun ft -> $1 ft }
  | app_like_expr atomic_expr { fun ft -> Just(App($1 ft, $2 ft)) }

atomic_expr:
  | LPAREN expr RPAREN { fun ft -> $2 ft }
  | IDENT { fun _ -> Just(Var (`Local $1)) }
  | NUM { fun _ -> Just(Nat $1) }
  | BOOL { fun _ -> Just(Bool $1) }

ty:
  | arrow_like_type { fun ft -> $1 ft }

arrow_like_type:
  | atomic_type { fun ft -> $1 ft }
  | atomic_type ARROW arrow_like_type { fun ft -> TArrow($1 ft, $3 ft) }

atomic_type:
  | LPAREN ty RPAREN { fun ft -> $2 ft }
  | TNAT   { fun _ -> TNat }
  | TBOOL  { fun _ -> TBool }
  | QMARK  { fun _ -> TUnknown }
  | USCORE { fun ft -> ft () }
