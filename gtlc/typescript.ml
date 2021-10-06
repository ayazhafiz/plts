open Lift_ir
open Util
open Builtin

let prelude =
  String.trim
    (Printf.sprintf
       {|
// #region Prelude
/** A package over a function. */
interface Fn<P, R> {
  apply(p: P): R
}
/** A closure over a function. */
class Clos<P, R> implements Fn<P, R> {
  constructor(private readonly target: (env: any, p: P) => R,
                private readonly args: unknown[]) {}

  apply(p: P): R {
    return this.target(this.args, p);
  }
}
/** A raw function pointer. */
class FnPtr<P, R> implements Fn<P, R> {
  constructor(private readonly target: (p: P) => R) {}
  
  apply(p: P): R {
    return this.target(p);
  }
}

type ty = 1|2|3|[ty,ty];
const _tn: 1 = 1;
const _tb: 2 = 2;
const _tu: 3 = 3;
function _tf(left: ty, right: ty): ty {return [left, right];}
interface v<T> {
  value: T,
  tag: ty;
}
function _nn(n : number): v<number> {
  return { value: n, tag: _tn };
}
function _nb(b : boolean): v<boolean> {
  return { value: b, tag: _tb };
}
function _nf<L, R>(clos : Fn<L, R>, tag: ty): v<Fn<L,R>> {
  return { value: clos, tag };
}
function _tag2s (tag: ty) : string {
  if (tag === _tb) return "bool";
  if (tag === _tn) return "number";
  if (tag === _tu) return "unknown";
  return `((_: ${_tag2s(tag[0])}) => ${_tag2s(tag[1])})`
}
function _consistent(s: ty, t: ty): boolean {
  if (s === _tu || t === _tu) return true;
  if (typeof s === 'number' && typeof t === 'number') return s === t;
  if (typeof s === 'number' || typeof t === 'number') return false;
  return _consistent(s[0], t[0]) && _consistent(s[1], t[1]);
}
function _cast<T, U>(value: v<T>, tag: ty): v<U> {
  if (_consistent(value.tag, tag)) return value as unknown as v<U>;
  throw new Error(`Cast Error: trying to cast ${value.value} as ${_tag2s(tag)}`);
}
function _print({value, tag}: v<any>) {
  if (value instanceof Clos || value instanceof FnPtr) {
    return `<Fn${_tag2s(tag)}>`;
  }
  return `${value}`;
}

function _succ(n: v<number>) {return _nn(n.value + 1);}
function _pred(n: v<number>) {return _nn(n.value - 1);}
function _add_captured(env: [v<number>], m: v<number>) {return _nn(env[0].value + m.value);}
function _add(n: v<number>) {
  return _nf(new Clos(_add_captured, [n]), _tf(_tn, _tn));
}
function _mult_captured(env: [v<number>], m: v<number>) {return _nn(env[0].value * m.value);}
function _mult(n: v<number>) {
  return _nf(new Clos(_mult_captured, [n]), _tf(_tn, _tn));
}
function _eqn_captured(env: [v<number>], m: v<number>) {return _nb(env[0].value === m.value);}
function _eqn(n: v<number>) {
  return _nf(new Clos(_eqn_captured, [n]), _tf(_tn, _tb));
}
function _eqb_captured(env: [v<boolean>], m: v<boolean>) {return _nb(env[0].value === m.value);}
function _eqb(n: v<boolean>) {
  return _nf(new Clos(_eqb_captured, [n]), _tf(_tb, _tb));
}
/** %s */
const succ = _nf(new FnPtr(_succ), _tf(_tn, _tn));
/** %s */
const pred = _nf(new FnPtr(_pred), _tf(_tn, _tn));
/** %s */
const add = _nf(new FnPtr(_add), _tf(_tn, _tf(_tn, _tn)));
/** %s */
const mult = _nf(new FnPtr(_mult), _tf(_tn, _tf(_tn, _tn)));
/** %s */
const eqn = _nf(new FnPtr(_eqn), _tf(_tn, _tf(_tn, _tb)));
/** %s */
const eqb = _nf(new FnPtr(_eqb), _tf(_tb, _tf(_tb, _tb)));
// #endregion
|}
       (doc_of_builtin "succ") (doc_of_builtin "pred") (doc_of_builtin "add")
       (doc_of_builtin "mult") (doc_of_builtin "eqn") (doc_of_builtin "eqb"))

let pp_tag f =
  let open Format in
  let rec go = function
    | TUnknown -> pp_print_string f "_tu"
    | TNat -> pp_print_string f "_tn"
    | TBool -> pp_print_string f "_tb"
    | TArrow (t, t') ->
        fprintf f "@[<hov 2>_tf(";
        go t;
        fprintf f ",@ ";
        go t';
        fprintf f ")@]"
    | TNamedTup _ -> failwith "unreachable"
  in
  go

let pp_ty f =
  let open Format in
  let rec go = function
    | TUnknown -> pp_print_string f "v<unknown>"
    | TNat -> pp_print_string f "v<number>"
    | TBool -> pp_print_string f "v<boolean>"
    | TArrow (t, t') ->
        fprintf f "v<@[<hov 2>Fn<";
        go t;
        fprintf f ",@ ";
        go t';
        fprintf f ">@]>"
    | TNamedTup ps ->
        let ts = List.map snd ps in
        fprintf f "[@[<hov 0>";
        let lasti = List.length ps - 1 in
        List.iteri
          (fun i t ->
            go t;
            if i <> lasti then fprintf f ",@ ")
          ts;
        fprintf f "@]]"
  in
  go

let pp_expr ts_ident f =
  let open Format in
  let rec go (Elab (e, _)) =
    match e with
    | Nat n -> fprintf f "_nn(%d)" n
    | Bool b -> fprintf f "_nb(%b)" b
    | Name x -> pp_print_string f (ts_ident x)
    | Apply (head, arg) ->
        fprintf f "@[<hov 2>";
        go head;
        fprintf f ".value.apply(@,";
        go arg;
        fprintf f ")@]"
    | Proj (x, i) ->
        fprintf f "@[";
        go x;
        fprintf f "[%d]@]" i
    | PackClos (fn, args) ->
        fprintf f "@[<hov 2>_nf(@[new Clos(@[<hv 0>";
        go fn;
        fprintf f ",@ ";
        fprintf f "[@[<hov 0>";
        let lasti = List.length args - 1 in
        List.iteri
          (fun i t ->
            go t;
            if i <> lasti then fprintf f ",@ ")
          args;
        fprintf f "@]]@])@],@ ";
        pp_tag f (Lift_ir.tyof fn);
        fprintf f ")@]"
    | PackFnPtr fn ->
        fprintf f "@[<hov 2>_nf(@[new FnPtr(@[<hv 0>";
        go fn;
        fprintf f "@])@],@ ";
        pp_tag f (Lift_ir.tyof fn);
        fprintf f ")@]"
  in
  go

let pp_stmt ts_ident f =
  let open Format in
  let rec go = function
    | Decl (x, t) ->
        fprintf f "let @[<hov 2>%s@,: " (ts_ident x);
        pp_ty f t;
        fprintf f ";@]"
    | DeclInit (x, t, e) ->
        fprintf f "const @[<hv 2>%s@,: " (ts_ident x);
        pp_ty f t;
        fprintf f "@ = ";
        pp_expr ts_ident f e;
        fprintf f ";@]"
    | DeclCast (x, t, e) ->
        fprintf f "const @[<hv 2>%s@,: " (ts_ident x);
        pp_ty f t;
        fprintf f "@ = @[<hov 2>_cast(";
        pp_expr ts_ident f e;
        fprintf f ",@ ";
        pp_tag f t;
        fprintf f ")@];@]"
    | Assign (x, e) ->
        fprintf f "@[<hv 2>%s@ = " (ts_ident x);
        pp_expr ts_ident f e;
        fprintf f ";@]"
    | If (c, t, e) ->
        fprintf f "@[<v 0>@[<v 2>@[<v 2>if (@[<hov 2>";
        pp_expr ts_ident f c;
        fprintf f "@,.value@])@] {@ ";
        let lasti = List.length t - 1 in
        List.iteri
          (fun i s ->
            go s;
            if i <> lasti then fprintf f "@,")
          t;
        fprintf f "@]@ }@[<v 1> else {@ ";
        let lasti = List.length e - 1 in
        List.iteri
          (fun i s ->
            go s;
            if i <> lasti then fprintf f "@,")
          e;
        fprintf f "@]@ }@]"
    | Return e ->
        fprintf f "@[<hov 2>return ";
        pp_expr ts_ident f e;
        fprintf f ";@]"
  in
  go

let pp_fn ts_ident f { name; params; body = Body stmts; ret } =
  let open Format in
  fprintf f "@[<v 0>@[<v 2>@[<hov 2>function %s(@[<hv 0>" (ts_ident name);
  let lasti = List.length params - 1 in
  List.iteri
    (fun i (p, t) ->
      fprintf f "@[<hov 2>%s:@ " (ts_ident p);
      pp_ty f t;
      fprintf f "@]";
      if i <> lasti then fprintf f ",@ ")
    params;
  fprintf f "@])@,: @[";
  pp_ty f ret;
  fprintf f "@]@] {@,@[<v 0>";
  let lasti = List.length stmts - 1 in
  List.iteri
    (fun i s ->
      pp_stmt ts_ident f s;
      if i <> lasti then fprintf f "@,")
    stmts;
  fprintf f "@]@]@ }@]"

let ts_ident_generator fresh =
  let mapping = Hashtbl.create 8 in
  fun ident ->
    let ident' = Str.global_replace (Str.regexp_string "'") "_" ident in
    if ident = ident' then ident
    else
      match Hashtbl.find_opt mapping ident with
      | Some ident' -> ident'
      | None ->
          let ident'' = fresh ident' in
          Hashtbl.add mapping ident ident'';
          ident''

let pp_program f { toplevels; body; ty; fresh } =
  let ts_ident = ts_ident_generator fresh in
  let main = fresh "main" in
  let fns = toplevels @ [ { name = main; params = []; body; ret = ty } ] in
  let open Format in
  fprintf f "@[<v 0>";
  List.iter
    (fun s ->
      pp_fn ts_ident f s;
      fprintf f "@,")
    fns;
  fprintf f "_print(%s());@]" (ts_ident main)

let string_of_program width with_prelude p =
  let prog = with_buffer (fun f -> pp_program f p) width in
  if with_prelude then prelude ^ "\n" ^ prog else prog
