%{
  open F
%}

%token <string> MACRO
%token <string> TYMACRO
%token <string> IDENT
%token <int>    NUM

%token FIX
%token INT
%token IF0
%token THEN
%token ELSE
%token LET
%token IN
%token EQ
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
%type <(string * [`VMac of term | `TMac of ty]) list -> term> toplevel_term
%%

toplevel_term:
  | LET MACRO EQ atomic_term IN toplevel_term { fun ctx ->
      let ctx' = ($2, `VMac ($4 ctx))::ctx in
      $6 ctx'
    } 
  | LET TYMACRO EQ atomic_type IN toplevel_term { fun ctx ->
      let ctx' = ($2, `TMac ($4 ctx))::ctx in
      $6 ctx'
    } 
  | term EOF { fun ctx -> $1 ctx }

term:
  | bigterm { fun ctx -> $1 ctx }
  | bigterm COLON ty { fun ctx -> Annot($1 ctx, $3 ctx) }

bigterm:
  | atomic_term LANGLE ty RANGLE { fun ctx -> TyApp($1 ctx, $3 ctx) }
  | atomic_term DOT NUM { fun ctx -> Proj($1 ctx, $3) }
  | binary_expr { fun ctx -> $1 ctx }
  | app_like_term { fun ctx -> $1 ctx }
  | FIX IDENT LPAREN IDENT COLON ty RPAREN COLON ty DOT term { fun ctx -> Fix {
      name=$2;
      param=$4;
      param_ty=$6 ctx;
      ret_ty=$9 ctx;
      body=$11 ctx } }
  | BIGLAM IDENT DOT term { fun ctx -> TyAbs($2, $4 ctx) }
  | IF0 term THEN term ELSE term { fun ctx -> If0($2 ctx, $4 ctx, $6 ctx) }

app_like_term:
  | atomic_term { fun ctx -> $1 ctx }
  | app_like_term atomic_term { fun ctx -> App ($1 ctx, $2 ctx) }

binary_expr:
  | term PLUS term { fun ctx -> Op (Plus, $1 ctx, $3 ctx) }
  | term MINUS term { fun ctx -> Op (Minus, $1 ctx, $3 ctx) }
  | term TIMES term { fun ctx -> Op (Times, $1 ctx, $3 ctx) }

atomic_term:
  | LPAREN term RPAREN { fun ctx -> $2 ctx }
  | LPAREN term COMMA tuple_term_rest { fun ctx -> Tup (($2 ctx)::($4 ctx)) }
  | MACRO { fun ctx -> match List.assoc $1 ctx with `VMac t -> t | _ -> failwith "unreachable" }
  | IDENT { fun _ -> Var $1 }
  | NUM { fun _ -> Int $1 }

tuple_term_rest:
  | term RPAREN { fun ctx -> [$1 ctx] }
  | term COMMA tuple_term_rest { fun ctx -> ($1 ctx)::($3 ctx) }

ty:
  | arrow_like_type { fun ctx -> $1 ctx }
  | FORALL IDENT DOT ty { fun ctx -> TAll ($2, $4 ctx) }

arrow_like_type:
  | atomic_type { fun ctx -> $1 ctx }
  | atomic_type ARROW arrow_like_type { fun ctx -> TArrow ($1 ctx, $3 ctx) }

atomic_type:
  | LPAREN ty RPAREN { fun ctx -> $2 ctx }
  | LPAREN ty COMMA tuple_type_rest { fun ctx -> TTup ($2 ctx::$4 ctx) }
  | TYMACRO { fun ctx -> match List.assoc $1 ctx with `TMac t -> t | _ -> failwith "unreachable" }
  | INT { fun _ -> TInt }
  | IDENT { fun _ -> TName $1 }

tuple_type_rest:
  | ty RPAREN { fun ctx -> [$1 ctx] }
  | ty COMMA tuple_type_rest { fun ctx -> ($1 ctx)::($3 ctx) }
