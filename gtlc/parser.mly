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
%token REF
%token DEREF
%token DEFEQ
%token SEMI

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
      Just(App(Just(Lam($2, TUnknown, $6 ft)), $4 ft, `DesugaredLet))
  }
  | LET IDENT COLON ty EQ expr IN expr { fun ft ->
      Just(App(Just(Lam($2, $4 ft, $8 ft)), $6 ft, `DesugaredLet))
  }
  | IF expr THEN expr ELSE expr { fun ft -> Just(If($2 ft, $4 ft, $6 ft)) }
  | REF atomic_expr { fun ft -> Just(Ref($2 ft)) }
  | expr DEFEQ expr { fun ft -> Just(RefAssign($1 ft, $3 ft)) }
  | expr DEFEQ expr SEMI expr { fun ft ->
      Just(
        App(Just(Lam("_", TUnknown, $5 ft)),
        Just(RefAssign($1 ft, $3 ft)),
        `DesugaredSeq))
  }

app_like_expr:
  | atomic_expr { fun ft -> $1 ft }
  | app_like_expr atomic_expr { fun ft -> Just(App($1 ft, $2 ft, `App)) }

atomic_expr:
  | LPAREN expr RPAREN { fun ft -> $2 ft }
  | IDENT { fun _ -> Just(Var (`Local $1)) }
  | NUM { fun _ -> Just(Nat $1) }
  | BOOL { fun _ -> Just(Bool $1) }
  | DEREF atomic_expr { fun ft -> Just(Deref($2 ft)) }

ty:
  | arrow_like_type { fun ft -> $1 ft }
  | REF atomic_type { fun ft -> TRef ($2 ft) }

arrow_like_type:
  | atomic_type { fun ft -> $1 ft }
  | atomic_type ARROW arrow_like_type { fun ft -> TArrow($1 ft, $3 ft) }

atomic_type:
  | LPAREN ty RPAREN { fun ft -> $2 ft }
  | TNAT   { fun _ -> TNat }
  | TBOOL  { fun _ -> TBool }
  | QMARK  { fun _ -> TUnknown }
  | USCORE { fun ft -> ft () }
