%{
open Ast

let range (start, _) (_, fin) = (start, fin)

let xloc = Ast.xloc
let xty = Ast.xty
let xv = Ast.xv

let with_name c name f =
  let old_name = !(c.opt_name) in
  c.opt_name := name;
  let r = f () in
  c.opt_name := old_name;
  r
%}

%token <Surface.loc * string> LOWER

%token <Surface.loc> LAMBDA
%token <Surface.loc> LPAREN
%token <Surface.loc> RPAREN
%token <Surface.loc> ARROW

%token EOF

%start toplevel
%type <Ast.parse_ctx -> Ast.program> toplevel
%type <Ast.parse_ctx -> Ast.e_expr> expr
%%

toplevel:
  | s=expr EOF { fun ctx -> s ctx }

expr:
  | e=expr_atom { fun c -> e c }
  | app=expr_app { fun c -> app c }
  | lam=LAMBDA arg=LOWER ARROW body=expr { fun ctx ->
      let arg_sym = ctx.symbols.fresh_symbol_named (snd arg) in
      Symbol.enter_scope ctx.symbols (snd arg) arg_sym;
      let body = with_name ctx None (fun () -> body ctx) in
      Symbol.exit_scope ctx.symbols (snd arg);
      let loc = range lam (xloc body) in
      (loc, None, Abs(arg_sym, body))
  }

expr_app:
  | e=expr_atom { fun ctx -> e ctx }
  | fn=expr_app arg=expr_atom { fun ctx ->
      let fn = fn ctx in
      let arg = arg ctx in
      let loc = range (xloc fn) (xloc arg) in
      (loc, None, App(fn, arg))
  }

expr_atom:
  | x=LOWER { fun ctx ->
      let loc_x = fst x in
      let x = Symbol.scoped_name ctx.symbols (snd x) in
      (loc_x, None, Var x)
  }
  | l=LPAREN e=expr r=RPAREN { fun ctx -> 
      let e = e ctx in
      (range l r, xty e, xv e)
  }
