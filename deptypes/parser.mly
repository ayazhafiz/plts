%{
  open Language
%}

%token <Language.info> LAMBDA
%token <Language.info> ALL
%token <Language.info> PI

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

Command :
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(dummyinfo ,t)),ctx }
  | UCID TyBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

/**
 ** BINDINGS
 **/
Binder :
    COLON Type
      { fun ctx -> VarBind ($2 ctx)}

TyBinder :
    /* empty */
      { fun _ -> TyVarBind(KnStar) }
  | COLONCOLON Kind
      { fun ctx -> TyVarBind($2 ctx) }

/**
 ** KINDS
 **/
Kind :
    ArrowKind
      { $1 }

ArrowKind :
    PI LCID COLON Type DOT Kind
      { fun ctx ->
        let ctx1 = addname ctx $2.v in
        KnPi($2.v, $4 ctx, $6 ctx1) }
  | AKind
           { $1 }

AKind :
    STAR { fun _ -> KnStar }
  | LPAREN Kind RPAREN  { $2 } 

/**
 ** TYPES
 **/
Type :
    ArrowType
                { $1 }
  | PI LCID COLON Type DOT Type
      { fun ctx ->
        let ctx1 = addname ctx $2.v in
        TyPi($2.v, $4 ctx, $6 ctx1) }

ArrowType :
    AppType
            { $1 }

AppType :
    AppType ATerm { fun ctx -> TyPiApp($1 ctx,$2 ctx) }
  | AType { $1 }

AType :
    LPAREN Type RPAREN  
           { $2 } 
  | UCID 
      { fun ctx ->
          TyVar(name2index ctx $1.v, ctxlength ctx) }

/**
 ** TERMS
 **/
Term :
    AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(dummyinfo ,e1,e2) }

ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index ctx $1.v, ctxlength ctx) }
