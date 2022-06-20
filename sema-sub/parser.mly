%{
open Syntax

let range (start, _) (_, fin) = (start, fin)
%}

%token <Syntax.loc> TANY
%token <Syntax.loc> TNEVER
%token <Syntax.loc> TINT
%token <Syntax.loc> TSTRING
%token <Syntax.loc> KTRUE
%token <Syntax.loc> KFALSE
%token <Syntax.loc> TBOOL
%token <Syntax.loc> TNOT
%token <Syntax.loc> TOR
%token <Syntax.loc> TAND
%token <Syntax.loc> LPAREN
%token <Syntax.loc> RPAREN
%token <Syntax.loc> COMMA
%token <Syntax.loc> ARROW
%token EOF

%left ARROW
%left TOR
%left TAND
%nonassoc TNOT

%start toplevel_ty
%type <unit -> Syntax.loc_ty> toplevel_ty
%%

toplevel_ty:
  | t=ty EOF { fun ctx -> t ctx }

ty:
  | l=LPAREN t=ty r=RPAREN { fun ctx -> (range l r, snd @@ t ctx) }
  | r=TANY    { fun _ -> (r, TAny) }
  | r=TNEVER  { fun _ -> (r, TNever) }
  | r=TINT    { fun _ -> (r, TInt) }
  | r=TSTRING { fun _ -> (r, TString) }
  | r=KTRUE   { fun _ -> (r, TTrue) }
  | r=KFALSE  { fun _ -> (r, TFalse) }
  | r=TBOOL   { fun _ -> (r, TOr ((r, TFalse), (r, TTrue))) }
  | t1=ty TOR t2=ty { fun ctx ->
      let t1, t2 = t1 ctx, t2 ctx in
      (range (fst t1) (fst t2), TOr(t1, t2)) }
  | t1=ty TAND t2=ty { fun ctx ->
      let t1, t2 = t1 ctx, t2 ctx in
      (range (fst t1) (fst t2), TAnd(t1, t2)) }
  | t1=ty ARROW t2=ty { fun ctx ->
      let t1, t2 = t1 ctx, t2 ctx in
      (range (fst t1) (fst t2), TArrow(t1, t2)) }
  | l=LPAREN t1=ty COMMA t2=ty r=RPAREN { fun ctx ->
      let t1, t2 = t1 ctx, t2 ctx in
      (range l r, TProd(t1, t2)) }
  | n=TNOT t=ty { fun ctx ->
      let t = t ctx in
      (range n (fst t), TNot t) }
