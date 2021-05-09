%{
  open Language
%}

%token <Language.info> LAMBDA
%token <Language.info> ALL

%token <string Language.withinfo> UCID  /* uppercase-initial */
%token <string Language.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Language.withinfo> INTV
%token <float Language.withinfo> FLOATV
%token <string Language.withinfo> STRINGV

%token <Language.info> APOSTROPHE
%token <Language.info> DQUOTE
%token <Language.info> ARROW
%token <Language.info> BANG
%token <Language.info> BARGT
%token <Language.info> BARRCURLY
%token <Language.info> BARRSQUARE
%token <Language.info> COLON
%token <Language.info> COLONCOLON
%token <Language.info> COLONEQ
%token <Language.info> COLONHASH
%token <Language.info> COMMA
%token <Language.info> DARROW
%token <Language.info> DDARROW
%token <Language.info> DOT
%token <Language.info> EOF
%token <Language.info> EQ
%token <Language.info> EQEQ
%token <Language.info> EXISTS
%token <Language.info> GT
%token <Language.info> HASH
%token <Language.info> LCURLY
%token <Language.info> LCURLYBAR
%token <Language.info> LEFTARROW
%token <Language.info> LPAREN
%token <Language.info> LSQUARE
%token <Language.info> LSQUAREBAR
%token <Language.info> LT
%token <Language.info> RCURLY
%token <Language.info> RPAREN
%token <Language.info> RSQUARE
%token <Language.info> SEMI
%token <Language.info> SLASH
%token <Language.info> STAR
%token <Language.info> TRIANGLE
%token <Language.info> USCORE
%token <Language.info> VBAR

%start toplevel
%type <Language.context -> (Language.command list * Language.context)> toplevel
%%

toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

/* A top-level command */
Command :
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(dummyinfo ,t)),ctx }
  | UCID TyBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type
      { fun ctx -> VarBind ($2 ctx)}

/* All kind expressions */
Kind :
    ArrowKind
      { $1 }

ArrowKind :
    AKind DARROW ArrowKind  { fun ctx -> KnArrow($1 ctx, $3 ctx) }
  | AKind
           { $1 }

/* All type expressions */
Type :
    ArrowType
                { $1 }
  | ALL UCID OKind DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAll($2.v,$3 ctx,$5 ctx1) }
  | LAMBDA UCID OKind DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAbs($2.v, $3 ctx, $5 ctx1) }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN  
           { $2 } 
  | UCID 
      { fun ctx ->
          TyVar(name2index ctx $1.v, ctxlength ctx) }

TyBinder :
    /* empty */
      { fun _ -> TyVarBind(KnStar) }
  | COLONCOLON Kind
      { fun ctx -> TyVarBind($2 ctx) }

AKind :
    STAR { fun _ -> KnStar }
  | LPAREN Kind RPAREN  { $2 } 

OKind :
  /* empty */
     { fun _ -> KnStar}
| COLONCOLON Kind 
     { $2 }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
    AppType ARROW ArrowType
     { fun ctx -> TyFn($1 ctx, $3 ctx) }
  | AppType
            { $1 }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | LAMBDA UCID OKind DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmTyAbs($1,$2.v,$3 ctx,$5 ctx1) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(dummyinfo ,e1,e2) }
  | AppTerm LSQUARE Type RSQUARE
      { fun ctx ->
          let t1 = $1 ctx in
          let t2 = $3 ctx in
          TmTyApp(dummyinfo ,t1,t2) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index ctx $1.v, ctxlength ctx) }

AppType :
    AppType AType { fun ctx -> TyApp($1 ctx,$2 ctx) }
  | AType { $1 }


/*   */
