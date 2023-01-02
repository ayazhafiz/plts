%{
open Ast

let range (start, _) (_, fin) = (start, fin)

let xloc = Ast.xloc
let xty = Ast.xty
let xv = Ast.xv
%}


%token <Surface.loc * string> LOWER

%token <Surface.loc * bool> LITBOOL
%token <Surface.loc * int> LITINT

%token <Surface.loc> LET
%token <Surface.loc> IN
%token <Surface.loc> IF
%token <Surface.loc> THEN
%token <Surface.loc> ELSE
%token <Surface.loc> LAMBDA
%token <Surface.loc> LPAREN
%token <Surface.loc> RPAREN
%token <Surface.loc> EQ
%token <Surface.loc> ARROW

%token EOF

%start toplevel
%type <Ast.parse_ctx -> Ast.program> toplevel
%type <Ast.parse_ctx -> Ast.e_stmt> stmt
%type <Ast.parse_ctx -> Ast.e_expr> expr
%%

toplevel:
  | s=stmt EOF { fun ctx -> s ctx }

stmt:
  | app=stmt_app { fun c -> app c }
  | lets=stmt_lets { fun c -> lets c }
  | e=expr { fun c -> (* return *)
      let e = e c in
      (xloc e, xty e, Return e)
  }
  | i=IF c=expr THEN t=expr ELSE e=expr { fun ctx ->
      let c = c ctx in
      let t = t ctx in
      let e = e ctx in
      let loc = range i (xloc e) in
      (loc, ctx.fresh_var (), If (c, t, e))
  }

stmt_app:
  | fn=expr_atom arg=expr_atom { fun ctx ->
      let fn = fn ctx in
      let arg = arg ctx in
      let loc = range (xloc fn) (xloc arg) in
      (loc, ctx.fresh_var(), App(fn, arg))
  }

stmt_lets:
  | l=LET loc_x=LOWER EQ e=stmt IN body=stmt { fun c ->
      let body = body c in
      let loc = range l (xloc body) in
      let x = (fst loc_x, c.fresh_var(), snd loc_x) in
      (loc, c.fresh_var(), Let(x, e c, body))
  }

expr:
  | e=expr_atom { fun c -> e c }
  | lam=LAMBDA arg=LOWER ARROW body=stmt { fun ctx ->
      let body = body ctx in
      let loc = range lam (xloc body) in
      let arg = (fst arg, ctx.fresh_var (), snd arg) in
      (loc, ctx.fresh_var (), Abs(arg, body))
  }

expr_atom:
  | x=LOWER { fun ctx -> (fst x, ctx.fresh_var (), Var (snd x)) }
  | b=LITBOOL { fun ctx -> (fst b, ctx.fresh_var (), Lit (`Bool (snd b))) }
  | l=LPAREN e=expr r=RPAREN { fun ctx -> 
      let e = e ctx in
      (range l r, xty e, xv e)
  }
