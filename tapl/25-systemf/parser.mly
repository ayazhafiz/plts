%{
open Util.Error
open Language
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Util.Error.info> TYPE
%token <Util.Error.info> INERT
%token <Util.Error.info> IF
%token <Util.Error.info> THEN
%token <Util.Error.info> ELSE
%token <Util.Error.info> TRUE
%token <Util.Error.info> FALSE
%token <Util.Error.info> BOOL
%token <Util.Error.info> CASE
%token <Util.Error.info> OF
%token <Util.Error.info> AS
%token <Util.Error.info> LAMBDA
%token <Util.Error.info> LET
%token <Util.Error.info> IN
%token <Util.Error.info> FIX
%token <Util.Error.info> LETREC
%token <Util.Error.info> USTRING
%token <Util.Error.info> UNIT
%token <Util.Error.info> UUNIT
%token <Util.Error.info> TIMESFLOAT
%token <Util.Error.info> PLUSFLOAT
%token <Util.Error.info> UFLOAT
%token <Util.Error.info> SUCC
%token <Util.Error.info> PRED
%token <Util.Error.info> ISZERO
%token <Util.Error.info> NAT
%token <Util.Error.info> NIL
%token <Util.Error.info> CONS
%token <Util.Error.info> ISNIL
%token <Util.Error.info> HEAD
%token <Util.Error.info> TAIL
%token <Util.Error.info> REF
%token <Util.Error.info> RREF
%token <Util.Error.info> ALL
%token <Util.Error.info> SOME

/* Identifier and constant value tokens */
%token <string Util.Error.withinfo> UCID  /* uppercase-initial */
%token <string Util.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Util.Error.withinfo> INTV
%token <float Util.Error.withinfo> FLOATV
%token <string Util.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Util.Error.info> APOSTROPHE
%token <Util.Error.info> DQUOTE
%token <Util.Error.info> ARROW
%token <Util.Error.info> BANG
%token <Util.Error.info> BARGT
%token <Util.Error.info> BARRCURLY
%token <Util.Error.info> BARRSQUARE
%token <Util.Error.info> COLON
%token <Util.Error.info> COLONCOLON
%token <Util.Error.info> COLONEQ
%token <Util.Error.info> COLONHASH
%token <Util.Error.info> COMMA
%token <Util.Error.info> DARROW
%token <Util.Error.info> DDARROW
%token <Util.Error.info> DOT
%token <Util.Error.info> EOF
%token <Util.Error.info> EQ
%token <Util.Error.info> EQEQ
%token <Util.Error.info> EXISTS
%token <Util.Error.info> GT
%token <Util.Error.info> HASH
%token <Util.Error.info> LCURLY
%token <Util.Error.info> LCURLYBAR
%token <Util.Error.info> LEFTARROW
%token <Util.Error.info> LPAREN
%token <Util.Error.info> LSQUARE
%token <Util.Error.info> LSQUAREBAR
%token <Util.Error.info> LT
%token <Util.Error.info> RCURLY
%token <Util.Error.info> RPAREN
%token <Util.Error.info> RSQUARE
%token <Util.Error.info> SEMI
%token <Util.Error.info> SLASH
%token <Util.Error.info> STAR
%token <Util.Error.info> TRIANGLE
%token <Util.Error.info> USCORE
%token <Util.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Language.context -> (Language.command list * Language.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Language.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Language.context -> (Language.command list * Language.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
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
    Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  /* A type binding, like "T = Nat -> Nat" */
  | UCID TyBinder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }
  /* A name binding, like "x" */
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type
      { fun ctx -> VarBinding ($2 ctx)}
  | EQ Term
      { fun ctx -> TmAbbBinding($2 ctx, None) }

/* All type expressions */
Type :
    ArrowType
                { $1 }
  | ALL UCID DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAll($2.v, $4 ctx1) }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN  
           { $2 } 
  | UCID
      { fun ctx ->
          if isNameBound ctx $1.v then
            TyVar(name2index $1.i ctx $1.v, ctxlength ctx)
          else
            TyId($1.v) }
  | LT FieldTypes GT
      { fun ctx -> TyVariant($2 ctx 1) }
  | LCURLY FieldTypes RCURLY
      { fun ctx -> TyRecord($2 ctx 1) }
  | BOOL
      { fun _ -> TyBool }
  | USTRING
      { fun _ -> TyString }
  | UUNIT
      { fun _ -> TyUnit }
  | UFLOAT
      { fun _ -> TyFloat }
  | NAT
      { fun _ -> TyNat }
  | RREF Type
      { fun ctx -> TyRef($2 ctx) }
  | LCURLY SOME UCID COMMA Type RCURLY
      { fun ctx ->
          let ctx1 = addname ctx $3.v in
          TySome($3.v, $5 ctx1) }

TyBinder:
    /* empty */
      { fun _ -> TyVarBinding }
  | EQ Type
      { fun ctx -> TyAbbBinding($2 ctx) }

FieldTypes:
    /* empty */
      { fun _ _ -> [] }
  | NEFieldTypes
      { $1 }

NEFieldTypes:
    FieldType
      { fun ctx i -> [$1 ctx i] }
  | FieldType COMMA NEFieldTypes
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

FieldType:
    LCID COLON Type
      { fun ctx _ -> ($1.v, $3 ctx) }
  /* Tuple: no field name */
  | Type
      { fun ctx i -> (string_of_int i, $1 ctx) }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
    AType ARROW ArrowType
     { fun ctx -> TyArrow($1 ctx, $3 ctx) }
  | AType
            { $1 }

Term :
    AppTerm
      { $1 }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
  | CASE Term OF Cases
      { fun ctx -> TmCase($1, $2 ctx, $4 ctx) }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA UCID DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmTyAbs($1, $2.v, $4 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | LET LCID EQ Term IN Term
      { fun ctx -> TmLet($1, $2.v, $4 ctx, $6 (addname ctx $2.v)) }
  | LET USCORE EQ Term IN Term
      { fun ctx -> TmLet($1, "_", $4 ctx, $6 (addname ctx "_")) }
  | LETREC LCID COLON Type EQ Term IN Term
      { fun ctx ->
          let ctx' = addname ctx $2.v in
          TmLet($1, $2.v, TmFix($1, TmAbs($1, $2.v, $4 ctx, $6 ctx')), $8 ctx') }
  | AppTerm COLONEQ AppTerm
      { fun ctx -> TmRefAssign($2, $1 ctx, $3 ctx) }
  | LET LCURLY UCID COMMA LCID RCURLY EQ Term IN Term
      { fun ctx ->
          let ctx1 = addname ctx $3.v in
          let ctx2 = addname ctx1 $5.v in
          TmUnpack($1,$3.v,$5.v,$8 ctx,$10 ctx2) }

AppTerm :
    PathTerm
      { $1 }
  | AppTerm PathTerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }
  | FIX PathTerm
      { fun ctx -> TmFix($1, $2 ctx) }
  | REF PathTerm
      { fun ctx -> TmRef($1, $2 ctx) }
  | BANG PathTerm
      { fun ctx -> TmDeref($1, $2 ctx) }
  | TIMESFLOAT PathTerm PathTerm
      { fun ctx -> TmTimesfloat($1, $2 ctx, $3 ctx) }
  | PLUSFLOAT PathTerm PathTerm
      { fun ctx -> TmPlusfloat($1, $2 ctx, $3 ctx) }
  | SUCC PathTerm
      { fun ctx -> TmSucc($1, $2 ctx) }
  | PRED PathTerm
      { fun ctx -> TmPred($1, $2 ctx) }
  | ISZERO PathTerm
      { fun ctx -> TmIsZero($1, $2 ctx) }
  | AppTerm LSQUARE Type RSQUARE
      { fun ctx ->
          let t1 = $1 ctx in
          let t2 = $3 ctx in
          TmTyApp(tmInfo t1,t1,t2) }

AscribeTerm:
    ATerm AS Type
      { fun ctx -> TmAscribe($2, $1 ctx, $3 ctx) }
  | ATerm
      { $1 }
 
PathTerm:
    PathTerm DOT LCID
      { fun ctx -> TmProj($2, $1 ctx, $3.v) }
  | PathTerm DOT INTV
      { fun ctx -> TmProj($2, $1 ctx, string_of_int $3.v) }
  | AscribeTerm
      { $1 }
 
TermSeq:
    Term
      { $1 }
  /* a; b -> (lambda _: b)(a) */
  | Term SEMI TermSeq
      { fun ctx -> TmApp($2, TmAbs($2, "_", TyUnit, $3 (addname ctx "_")), $1 ctx) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN TermSeq RPAREN  
      { $2 } 
  | INERT LSQUARE Type RSQUARE
      { fun ctx -> TmInert($1, $3 ctx) }
  | TRUE
      { fun _ -> TmTrue($1) }
  | FALSE
      { fun _ -> TmFalse($1) }
  | LT LCID EQ Term GT AS Type
      { fun ctx -> TmTag($1, $2.v, $4 ctx, $7 ctx) }
  | LCID 
      { fun ctx -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | LCURLY Fields RCURLY 
      { fun ctx -> TmRecord($1, $2 ctx 1) }
  /* | LSQUARE ListTerms RSQUARE
         { fun ctx -> TmList($1, $2 ctx) } */
  | STRINGV 
      { fun _ -> TmString($1.i, $1.v) }
  | FLOATV 
      { fun _ -> TmFloat($1.i, $1.v) }
  | INTV 
      { fun _ ->
          let rec f n = match n with
            | 0 -> TmZero($1.i)
            | n -> TmSucc($1.i, f (n - 1))
          in f $1.v }
  | UNIT 
      { fun _ -> TmUnit($1) }
  | LCURLY STAR Type COMMA Term RCURLY AS Type
      { fun ctx ->
          TmPack($1, $3 ctx, $5 ctx, $8 ctx) }

Cases:
    Case
      { fun ctx -> [$1 ctx] }
  | Case VBAR Cases
      { fun ctx -> ($1 ctx) :: ($3 ctx) }

Case:
    LT LCID EQ LCID GT DDARROW AppTerm
      { fun ctx ->
          let ctx' = addname ctx $4.v in
          ($2.v, ($4.v, $7 ctx')) }

Fields:
    /* empty */
      { fun _ _ -> [] }
  | NEFields
      { $1 }

NEFields:
    Field
      { fun ctx i -> [$1 ctx i] }
  | Field COMMA NEFields
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field:
    LCID EQ Term
      { fun ctx _ -> ($1.v, $3 ctx) }
  /* Tuple: no field name */
  | Term
      { fun ctx i -> (string_of_int i, $1 ctx) }

/*
ListTerms:
      { fun _ _ -> [] }
  | NETerms
      { $1 }

NETerms:
    Term
      { fun ctx -> [$1 ctx] }
  | Term COMMA NEFields
      { fun ctx -> ($1 ctx) :: ($3 ctx) }
*/
