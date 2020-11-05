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
%token <Util.Error.info> LAMBDA
%token <Util.Error.info> IF
%token <Util.Error.info> THEN
%token <Util.Error.info> ELSE
%token <Util.Error.info> TRUE
%token <Util.Error.info> FALSE
%token <Util.Error.info> BOOL

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
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type
      { fun ctx -> VarBinding ($2 ctx)}

/* All type expressions */
Type :
    ArrowType
                { $1 }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN  
           { $2 } 
  | BOOL
      { fun _ -> TyBool }

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
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | TRUE
      { fun _ -> TmTrue($1) }
  | FALSE
      { fun _ -> TmFalse($1) }


/*   */
