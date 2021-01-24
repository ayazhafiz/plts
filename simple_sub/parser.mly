%token <bool>   BOOL
%token <int>    NAT
%token <string> STRING
%token <string> IDENT

%token FN
%token IF THEN ELSE
%token IS IN
%token TYPE_UNIT TYPE_BOOL TYPE_NAT TYPE_STRING
%token LPAREN RPAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LCURLY RCURLY
%token COMMA
%token COLON
%token SEMI
%token VBAR
%token DOT

%token MODE
%token EOF

%start toplevel
%type <int> toplevel
%%

toplevel:
  | COLON MODE IDENT EOF   { 1 }

/* Program:
/*   | EOF                          { { fns=[]; expr=None } }
/*   | e = Expr; EOF                { { fns=[]; expr=Some e } }
/*   | f = Func; prog = Program     { { prog with fns=f::prog.fns } }
/* ;
/* 
/* Func:
/*   | FN; name = IDENT; LPAREN; p = Params; RPAREN COLON; ty = Type; LCURLY; e = Expr; RCURLY
/*     { Fn (name, p, ty, e) }
/* ;
/* 
/* Type:
/*   | AtomicType  { $1 }
/*   | AtomicType VBAR AtomicType UnionSeqType { TyUnion ($1::$3::$4) }
/* ;
/* 
/* AtomicType:
/*   | TYPE_NAT              { TyNat }
/*   | TYPE_STRING           { TyString }
/*   | TYPE_BOOL             { TyBool }
/*   | LCURLY RcdType RCURLY { TyRecord($2) }
/*   | LPAREN Type RPAREN    { $2 }
/* ;
/* 
/* UnionSeqType:
/*   | { [] }
/*   | VBAR AtomicType UnionSeqType { $2::$3 }
/* ;
/* 
/* RcdType:
/*   | { [] }
/*   | IDENT COLON Type { [($1, $3)] }
/*   | IDENT COLON Type COMMA; rest = RcdType { ($1, $3)::rest }
/* ;
/* 
/* Params:
/*   | { [] }
/*   | id = IDENT; COLON; ty = Type { [(id, ty)] }  
/*   | id = IDENT; COLON; ty = Type; COMMA; rest = Params { (id, ty)::rest }
/* ;
/* 
/* AtomicExpr:
/*   | IDENT  { Var $1 }
/*   | NAT    { Nat $1 }
/*   | STRING { String $1 }
/*   | BOOL   { Bool $1 }
/*   | IDENT LPAREN ArgList RPAREN
/*            { App (Var $1, $3) }
/*   | LCURLY RcdList RCURLY
/*            { Record($2) }
/*   | LPAREN Expr RPAREN
/*            { $2 }
/*   | AtomicExpr DOT IDENT
/*            { RecordProj($1, $3) }
/*   /* Can only narrow variables; pointless to do so on values directly (why?) */
/*   | IDENT IS Type
/*            { Narrow((Var $1), $3) }
/*   | IDENT IN IDENT
/*            { RecordNarrow($1, (Var $3)) }
/* ;
/* 
/* Expr:
/*   | AtomicExpr { $1 }
/*   | IF AtomicExpr THEN Expr ELSE Expr { If($2, $4, $6) }
/* ;
/* 
/* ArgList:
/*   | { [] }
/*   | expr = Expr { [expr] }
/*   | expr = Expr; COMMA; rest = ArgList { expr::rest }
/* ;
/* 
/* RcdList:
/*   | { [] }
/*   | IDENT COLON Expr { [($1, $3)] }
/*   | IDENT COLON Expr COMMA; rest = RcdList { ($1, $3)::rest }
/* ; */
