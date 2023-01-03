%{
open Ast

let range (start, _) (_, fin) = (start, fin)

let xloc = Ast.xloc
let xty = Ast.xty
let xv = Ast.xv
%}


%token <Surface.loc * string> LOWER
%token <Surface.loc * string> UPPER

%token <Surface.loc * bool> LITBOOL
%token <Surface.loc * int> LITINT

%token <Surface.loc * Ast.builtin> BUILTIN

%token <Surface.loc> LET
%token <Surface.loc> REC
%token <Surface.loc> IN
%token <Surface.loc> IF
%token <Surface.loc> THEN
%token <Surface.loc> ELSE
%token <Surface.loc> HANDLE
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
  | i=IF c=stmt THEN t=stmt ELSE e=stmt { fun ctx ->
      let c = c ctx in
      let t = t ctx in
      let e = e ctx in
      let loc = range i (xloc e) in
      (loc, ctx.fresh_var (), If (c, t, e))
  }
  | kwh=HANDLE c=LOWER EQ h=cap IN s=stmt { fun ctx ->
      let c = (fst c, ctx.fresh_fx_var(), snd c) in
      let h = h ctx in
      let s = s ctx in
      let loc = range kwh (xloc s) in
      let handle = Handle(c, h, s) in
      (loc, ctx.fresh_var (), handle)
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
      (loc, c.fresh_var(), Let(`Rec false, x, e c, body))
  }
  | l=LET REC loc_x=LOWER EQ e=stmt IN body=stmt { fun c ->
      let body = body c in
      let loc = range l (xloc body) in
      let x = (fst loc_x, c.fresh_var(), snd loc_x) in
      (loc, c.fresh_var(), Let(`Rec true, x, e c, body))
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
  | n=LITINT { fun ctx -> (fst n, ctx.fresh_var (), Lit (`Int (snd n))) }
  | b=BUILTIN { fun ctx -> (fst b, ctx.fresh_var (), Builtin (snd b)) }
  | l=LPAREN e=expr r=RPAREN { fun ctx -> 
      let e = e ctx in
      (range l r, xty e, xv e)
  }

cap :
  | c=cap_atom { fun ctx -> c ctx }
  | op=UPPER x=LOWER k=LOWER ARROW body=stmt { fun ctx ->
      let fx_op = (fst op, ref @@
        Content (`Fx (snd op, (ctx.fresh_var(), ctx.fresh_var()))))
      in
      let x = (fst x, ctx.fresh_var (), snd x) in
      let k_fx_op = ref @@ Content
        (`Fx (ctx.fresh_resume_name(), (ctx.fresh_var(), ctx.fresh_var())))
      in
      let k = (fst k, k_fx_op, snd k) in
      let body = body ctx in
      let loc = range (fst op) (xloc body) in
      (loc, ctx.fresh_var (), HandlerImpl (fx_op, (x, k), body))
  }

cap_atom :
  | x=LOWER { fun ctx -> (fst x, ctx.fresh_var (), CapVar (snd x)) }
