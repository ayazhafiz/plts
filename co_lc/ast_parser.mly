%{
open Ast

let noloc = Ast.noloc

let range (start, _) (_, fin) = (start, fin)

let xloc = Ast.xloc
let xty = Ast.xty
let xv = Ast.xv
%}


%token <Surface.loc * string> LOWER

%token <Surface.loc * bool> LITBOOL
%token <Surface.loc * int> LITINT

%token <Surface.loc> LET
%token <Surface.loc> REC
%token <Surface.loc> IN
%token <Surface.loc> IF
%token <Surface.loc> THEN
%token <Surface.loc> ELSE
%token <Surface.loc> SPAWN
%token <Surface.loc> YIELD
%token <Surface.loc> RESUME
%token <Surface.loc> STAT
%token <Surface.loc> LAMBDA
%token <Surface.loc> LPAREN
%token <Surface.loc> RPAREN
%token <Surface.loc> LBRACE
%token <Surface.loc> RBRACE
%token <Surface.loc> EQ
%token <Surface.loc> ARROW
%token <Surface.loc> COMMA
%token <Surface.loc> SEMI
%token <Surface.loc> DOT
%token <Surface.loc> PIPE
%token <Surface.loc> TAG_PENDING
%token <Surface.loc> TAG_DONE

%token <Surface.loc> LT
%token <Surface.loc> PLUS
%token <Surface.loc> MINUS
%token <Surface.loc> MUL

%token EOF

%start toplevel
%type <Ast.parse_ctx -> Ast.program> toplevel
%type <Ast.parse_ctx -> Ast.e_expr> expr
%type <Ast.binop> binop
%%

toplevel:
  | s=expr EOF { fun ctx -> s ctx }

expr:
  | e=expr_atom { fun c -> e c }
  | app=expr_app { fun c -> app c }
  | binop=expr_binop { fun ctx -> binop ctx }
  | lets=expr_lets { fun c -> lets c }
  | lam=LAMBDA arg=LOWER ARROW body=expr { fun ctx ->
      let loc_arg = fst arg in
      let arg_sym = ctx.symbols.fresh_symbol_scoped (snd arg) in
      let body = body ctx in
      Symbol.exit_scope ctx.symbols (snd arg);
      let arg = (loc_arg, ctx.fresh_var (), arg_sym) in
      let loc = range lam (xloc body) in
      (loc, ctx.fresh_var (), Abs(arg, body))
  }
  | i=IF c=expr THEN t=expr ELSE e=expr { fun ctx ->
      let c = c ctx in
      let t = t ctx in
      let e = e ctx in
      let loc = range i (xloc e) in
      (loc, ctx.fresh_var (), If (c, t, e))
  }
  | s=STAT cond=expr PIPE TAG_PENDING ARROW pending=expr PIPE TAG_DONE n=LOWER ARROW done_body=expr { fun ctx ->
      let cond = cond ctx in
      let pending = pending ctx in

      let loc_n = fst n in
      let n_sym = ctx.symbols.fresh_symbol_scoped (snd n) in
      let done_body = done_body ctx in
      Symbol.exit_scope ctx.symbols (snd n);
      let n = (loc_n, ctx.fresh_var(), n_sym) in
      let loc = range s (xloc done_body) in
      (loc, ctx.fresh_var(), Stat { cond; pending; done'=(n, done_body) })
  }

expr_app:
  | e=expr_atom { fun ctx -> e ctx }
  | s=SPAWN arg=expr_atom { fun ctx ->
      let arg = arg ctx in
      let loc = range s (xloc arg) in
      (loc, ctx.fresh_var(), Spawn arg)
  }
  | r=RESUME arg=expr_atom { fun ctx ->
      let arg = arg ctx in
      let loc = range r (xloc arg) in
      (loc, ctx.fresh_var(), Resume arg)
  }
  | fn=expr_app arg=expr_atom { fun ctx ->
      let fn = fn ctx in
      let arg = arg ctx in
      let loc = range (xloc fn) (xloc arg) in
      (loc, ctx.fresh_var(), App(fn, arg))
  }

expr_binop:
  | l=expr b=binop r=expr { fun ctx ->
      let l = l ctx in
      let r = r ctx in
      let loc = range (xloc l) (xloc r) in
      (loc, ctx.fresh_var(), Binop(b, l, r))
  }

expr_lets:
  | l=LET loc_x=LOWER EQ e=expr IN body=expr { fun c ->
      let e = e c in
      let x_sym = c.symbols.fresh_symbol_scoped (snd loc_x) in
      let body = body c in
      Symbol.exit_scope c.symbols (snd loc_x);
      let loc = range l (xloc body) in
      let x = (fst loc_x, c.fresh_var(), x_sym) in
      (loc, c.fresh_var(), Let(`Bare, x, e, body))
  }
  | l=LET REC loc_x=LOWER EQ e=expr IN body=expr { fun c ->
      let x_sym = c.symbols.fresh_symbol_scoped (snd loc_x) in
      let e = e c in
      let body = body c in
      Symbol.exit_scope c.symbols (snd loc_x);
      let loc = range l (xloc body) in
      let x = (fst loc_x, c.fresh_var(), x_sym) in
      (loc, c.fresh_var(), Let(`Rec, x, e, body))
  }
  | e=expr SEMI rest=expr { fun c ->
      let noname = (noloc, c.fresh_var(), c.symbols.fresh_symbol "") in
      let e = e c in
      let rest = rest c in
      let loc = range (xloc e) (xloc rest) in
      (loc, c.fresh_var(), Let(`Unit, noname, e, rest))
  }

expr_atom:
  | x=LOWER { fun ctx ->
      let loc_x = fst x in
      let x = Symbol.scoped_name ctx.symbols (snd x) in
      (loc_x, ctx.fresh_var (), Var x)
  }
  | b=LITBOOL { fun ctx -> (fst b, ctx.fresh_var (), Lit (`Bool (snd b))) }
  | n=LITINT { fun ctx -> (fst n, ctx.fresh_var (), Lit (`Int (snd n))) }
  | l=LBRACE tup=expr_tup { fun ctx ->
      let (tup, loc_end) = tup ctx in
      let loc = range l loc_end in
      (loc, ctx.fresh_var(), Tup tup)
  }
  | y=YIELD { fun ctx -> (y, ctx.fresh_var(), Yield) }
  | l=LPAREN e=expr r=RPAREN { fun ctx -> 
      let e = e ctx in
      (range l r, xty e, xv e)
  }
  | e=expr_atom DOT n=LITINT { fun ctx ->
      let e = e ctx in
      let loc = range (xloc e) (fst n) in
      (loc, ctx.fresh_var(), Access(e, snd n))
  }

expr_tup:
  | r=RBRACE { fun _ -> ([], r) }
  | e=expr r=RBRACE { fun ctx -> ([e ctx], r) }
  | e=expr COMMA rest=expr_tup { fun ctx ->
      let e = e ctx in
      let (rest, r) = rest ctx in
      (e::rest, r)
  }

binop:
  | LT    { `Lt }
  | PLUS  { `Add }
  | MINUS { `Sub }
  | MUL   { `Mul }
