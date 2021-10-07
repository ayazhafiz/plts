open Lift_ir
open Builtin
open Util

let prelude =
  String.trim
    (Printf.sprintf
       {|
#pragma region Prelude
#include <memory.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t typekind;
#define UNKNOWN 0
#define NAT 1
#define BOOL 2
#define FN 3
#define BOX 4

typedef struct type {
  const typekind kind;
  const void* const handle;
} type;

typedef struct fn {
  const type* const left;
  const type* const right;
} fn;

typedef struct box {
  const type* const inner;
} box;

typedef uint8_t valuekind;
#define VNAT 0
#define VBOOL 1
#define VFN 2
#define VCLOS 3
#define VBOX 4

typedef struct v v;

typedef union rawval {
  const uint64_t nat;
  const uint8_t boolean;
  const v (*fn)(const v);
  const v (*clos)(const v* const, const v);
  v* boxed;
} rawval;

typedef struct v {
  const valuekind kind;
  const rawval val;
  const type* const tag;
  const v* const env;
} v;

#define _false 0
#define _true 1

uint8_t _is_consistent(const type t, const type u) {
  typekind tk = t.kind;
  typekind uk = u.kind;
  if (tk == UNKNOWN || uk == UNKNOWN) return 1;
  if (tk == FN && uk == FN) {
    const fn* const thandle = t.handle;
    const fn* const uhandle = u.handle;
    return _is_consistent(*thandle->left, *uhandle->left) &&
           _is_consistent(*thandle->right, *uhandle->right);
  }
  return tk == uk;
}

void _print_type(const type t) {
  switch (t.kind) {
    case UNKNOWN:
      printf("?");
      break;
    case NAT:
      printf("nat");
      break;
    case BOOL:
      printf("bool");
      break;
    case FN: {
      const fn* const handle = t.handle;
      printf("(");
      _print_type(*handle->left);
      printf(" -> ");
      _print_type(*handle->right);
      printf(")");
      break;
    case BOX: {
      const box* const handle = t.handle;
      printf("(box ");
      _print_type(*handle->inner);
      printf(")");
      break;
    }
    }
    default:
      printf("[UNHANDLED]");
  }
}

void _print_v(const v v) {
  switch (v.kind) {
    case VFN:
    case VCLOS: {
      printf("<Fn");
      _print_type(*v.tag);
      printf(">");
      break;
    }
    case VNAT:
      printf("%%llu", v.val.nat);
      break;
    case VBOOL:
      printf(v.val.boolean == _false ? "false" : "true");
      break;
    case VBOX:
      printf("&");
      _print_v(*v.val.boxed);
      break;
    default:
      printf("[UNHANDLED]");
  }
}

void _print(const v v) { _print_v(v); }

const v _cast(const v v, const type t) {
  if (_is_consistent(*v.tag, t)) return v;
  printf("Cast Error: attempting to cast ");
  _print_v(v);
  printf(" as ");
  _print_type(t);
  exit(1);
}

const v _apply(const v fn, const v arg) {
  if (fn.kind == VFN) return fn.val.fn(arg);
  if (fn.kind == VCLOS) return fn.val.clos(fn.env, arg);
  printf("Type Error: not a function");
  exit(1);
}

const type _tunknown = {.kind = UNKNOWN, .handle = NULL};
const type _tnat = {.kind = NAT, .handle = NULL};
const type _tbool = {.kind = BOOL, .handle = NULL};

/** Creates a new nat. */
v _nn(const uint64_t n) {
  rawval rv = {.nat = n};
  v val = {.kind = VNAT, .val = rv, .tag = &_tnat, .env = NULL};
  return val;
}

/** Creates a new boolean. */
v _nb(const uint8_t b) {
  rawval rv = {.boolean = b};
  v val = {.kind = VBOOL, .val = rv, .tag = &_tbool, .env = NULL};
  return val;
}

/** Creates a new raw function. */
v _nfn(const v (*fn)(const v), const type* const tag) {
  rawval rv = {.fn = fn};
  v val = {.kind = VFN, .val = rv, .tag = tag, .env = NULL};
  return val;
}

/** Creates a new closure. */
v _nclos(const v (*clos)(const v* const, const v), const type* const tag,
         const uint32_t env_length, ...) {
  v* env = malloc(env_length * sizeof(v));
  va_list lst;
  va_start(lst, env_length);
  for (uint64_t i = 0; i < env_length; ++i) {
    const v var = va_arg(lst, v);
    memcpy(&env[i], &var, sizeof(v));
  }

  rawval rv = {.clos = clos};
  v val = {.kind = VCLOS, .val = rv, .tag = tag, .env = env};
  return val;
}

/** Creates a new boxed value. */
v _nbox(const v val, const type* const tag) {
    v* box = malloc(sizeof(v));
    memcpy(box, &val, sizeof(v));
    rawval rv = {.boxed = box};
    v res = {.kind = VBOX, .val = rv, .tag = tag, .env = NULL};
    return res;
}

uint64_t _getn(v n) { return n.val.nat; }
uint8_t _getb(v n) { return n.val.boolean; }

v _boxP(v box, v newval) {
    memcpy(box.val.boxed, &newval, sizeof(v));
    return box;
}

const fn _fnnat_nat = {.left = &_tnat, .right = &_tnat};
const type _tnat_nat = {.kind = FN, .handle = &_fnnat_nat};

const fn _fnnat_bool = {.left = &_tnat, .right = &_tbool};
const type _tnat_bool = {.kind = FN, .handle = &_fnnat_bool};

const fn _fnbool_bool = {.left = &_tbool, .right = &_tbool};
const type _tbool_bool = {.kind = FN, .handle = &_fnbool_bool};

const fn _fnnat_nat_nat = {.left = &_tnat, .right = &_tnat_nat};
const type _tnat_nat_nat = {.kind = FN, .handle = &_fnnat_nat_nat};

const fn _fnnat_nat_bool = {.left = &_tnat, .right = &_tnat_bool};
const type _tnat_nat_bool = {.kind = FN, .handle = &_fnnat_nat_bool};

const fn _fnbool_bool_bool = {.left = &_tbool, .right = &_tbool_bool};
const type _tbool_bool_bool = {.kind = FN, .handle = &_fnbool_bool_bool};

const v _succ(const v n) { return _nn(_getn(n) + 1); }
const v _pred(const v n) { return _nn(_getn(n) - 1); }
const v _add_captured(const v* const env, const v n) {
  return _nn(_getn(env[0]) + _getn(n));
}
const v _add(const v m) { return _nclos(_add_captured, &_tnat_nat, 1, m); }
const v _mult_captured(const v* const env, const v n) {
  return _nn(_getn(env[0]) * _getn(n));
}
const v _mult(const v m) { return _nclos(_mult_captured, &_tnat_nat, 1, m); }
const v _eqn_captured(const v* const env, const v n) {
  return _nb(_getn(env[0]) == _getn(n));
}
const v _eqn(const v m) { return _nclos(_eqn_captured, &_tnat_bool, 1, m); }
const v _eqb_captured(const v* const env, const v c) {
  return _nb(_getb(env[0]) == _getb(c));
}
const v _eqb(const v b) { return _nclos(_eqn_captured, &_tbool_bool, 1, b); }

/** %s */
const v succ = {
    .kind = VFN, .val = {.fn = _succ}, .tag = &_tnat_nat, .env = NULL};
/** %s */
const v pred = {
    .kind = VFN, .val = {.fn = _pred}, .tag = &_tnat_nat, .env = NULL};
/** %s */
const v add = {
    .kind = VFN, .val = {.fn = _add}, .tag = &_tnat_nat_nat, .env = NULL};
/** %s */
const v mult = {
    .kind = VFN, .val = {.fn = _mult}, .tag = &_tnat_nat_nat, .env = NULL};
/** %s */
const v eqn = {
    .kind = VFN, .val = {.fn = _eqn}, .tag = &_tnat_nat_bool, .env = NULL};
/** %s */
const v eqb = {
    .kind = VFN, .val = {.fn = _eqb}, .tag = &_tbool_bool_bool, .env = NULL};
#pragma endregion
|}
       (doc_of_builtin "succ") (doc_of_builtin "pred") (doc_of_builtin "add")
       (doc_of_builtin "mult") (doc_of_builtin "eqn") (doc_of_builtin "eqb"))

type cgen = {
  fresh : string -> string;
  ident : string -> string;
  get_tag : ty -> string;
  f : Format.formatter;
}

let pp_expr { ident; get_tag; f; _ } =
  let open Format in
  let rec go (Elab (e, t)) =
    match e with
    | Nat n -> fprintf f "_nn(%d)" n
    | Bool true -> fprintf f "_nb(_true)"
    | Bool false -> fprintf f "_nb(_false)"
    | Name x -> pp_print_string f (ident x)
    | Apply (head, arg) ->
        fprintf f "@[<hov 2>_apply(@[";
        go head;
        fprintf f ",@ ";
        go arg;
        fprintf f "@])@]"
    | Proj (x, i) ->
        fprintf f "@[";
        go x;
        fprintf f "[%d]@]" i
    | PackClos (fn, args) ->
        fprintf f "@[<hov 2>_nclos(@[";
        go fn;
        fprintf f ",@ &%s,@ %d,@ "
          (get_tag (Lift_ir.tyof fn))
          (List.length args);
        let lasti = List.length args - 1 in
        List.iteri
          (fun i t ->
            go t;
            if i <> lasti then fprintf f ",@ ")
          args;
        fprintf f "@])@]"
    | PackFnPtr fn ->
        fprintf f "@[<hov 2>_nfn(@[";
        go fn;
        fprintf f ",@ &%s@])@]" (get_tag (Lift_ir.tyof fn))
    | Box v ->
        fprintf f "@[<hov 2>_nbox(@[";
        go v;
        fprintf f ",@ &%s@])@]" (get_tag t)
    | BoxEnplace (box, v) ->
        fprintf f "@[<hov 2>_boxP(@[";
        go box;
        fprintf f ",@ ";
        go v;
        fprintf f "@])@]"
    | Unbox box ->
        fprintf f "@[(*";
        go box;
        fprintf f ".val.boxed)@]"
  in
  go

let pp_stmt ({ fresh; ident; get_tag; f } as cgen) =
  let open Format in
  let rec go = function
    | Decl (x, _) -> fprintf f "@[<hov 2>v %s;@]" (ident x)
    | DeclInit (x, _, e) ->
        fprintf f "@[<hv 2>const v %s@ = " (ident x);
        pp_expr cgen e;
        fprintf f ";@]"
    | DeclCast (x, t, e) ->
        fprintf f "@[<hv 2>const v %s@ = @[<hov 2>_cast(" (ident x);
        pp_expr cgen e;
        fprintf f ",@ %s)@];@]" (get_tag t)
    | Assign (x, e) ->
        let y = fresh "y" in
        fprintf f "@[<hv 2>const v %s@ = " y;
        pp_expr cgen e;
        fprintf f ";@]@,";
        fprintf f "@[<hv 2>memcpy(&%s,@ &%s,@ sizeof(v));@]" (ident x) y
    | If (c, t, e) ->
        fprintf f "@[<v 0>@[<v 2>@[<v 2>if (@[<hov 2>_getb(@,";
        pp_expr cgen c;
        fprintf f ")@])@] {@ ";
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
        pp_expr cgen e;
        fprintf f ";@]"
  in
  go

let pp_fn ({ ident; f; _ } as cgen) { name; params; body = Body stmts; _ } =
  let open Format in
  fprintf f "@[<v 0>@[<v 2>@[<hov 2>const v %s(@[<hv 0>" (ident name);
  let lasti = List.length params - 1 in
  List.iteri
    (fun i (p, t) ->
      let ty = match t with TNamedTup _ -> "v* const" | _ -> "v" in
      fprintf f "@[<hov 2>const %s@ %s@]" ty (ident p);
      if i <> lasti then fprintf f ",@ ")
    params;
  fprintf f "@])@,";
  fprintf f "@] {@,@[<v 0>";
  let lasti = List.length stmts - 1 in
  List.iteri
    (fun i s ->
      pp_stmt cgen s;
      if i <> lasti then fprintf f "@,")
    stmts;
  fprintf f "@]@]@ }@]"

let c_ident_generator fresh =
  let mapping = Hashtbl.create 8 in
  fun ident ->
    let ident' =
      if ident = "main" then fresh "main"
      else Str.global_replace (Str.regexp_string "'") "_" ident
    in
    if ident = ident' then ident
    else
      match Hashtbl.find_opt mapping ident with
      | Some ident' -> ident'
      | None ->
          let ident'' = fresh ident' in
          Hashtbl.add mapping ident ident'';
          ident''

let c_get_tag_generator fresh add_decl =
  let open Printf in
  let builtin =
    [
      (TArrow (TNat, TNat), "_tnat_nat");
      (TArrow (TNat, TBool), "_tnat_bool");
      (TArrow (TBool, TBool), "_tbool_bool");
      (TArrow (TNat, TArrow (TNat, TNat)), "_tnat_nat_nat");
      (TArrow (TNat, TArrow (TNat, TBool)), "_tnat_nat_bool");
      (TArrow (TBool, TArrow (TBool, TBool)), "_tbool_bool_bool");
    ]
  in
  let store = Hashtbl.of_seq (List.to_seq builtin) in
  let rec getty = function
    | TUnknown -> "_tunknown"
    | TNat -> "_tnat"
    | TBool -> "_tbool"
    | TArrow _ as ty when Hashtbl.mem store ty -> Hashtbl.find store ty
    | TArrow (lty, rty) as ty ->
        let l = getty lty in
        let r = getty rty in
        let handle = fresh "_fn" in
        add_decl
          (sprintf "const fn %s = {.left = &%s, .right = &%s};" handle l r);
        let t = fresh "_t" in
        add_decl
          (sprintf "const type %s = {.kind = FN, .handle = &%s};" t handle);
        Hashtbl.add store ty t;
        t
    | TBox ity as ty ->
        let i = getty ity in
        let handle = fresh "_box" in
        add_decl (sprintf "const box %s = {.inner = &%s};" handle i);
        let t = fresh "_t" in
        add_decl
          (sprintf "const type %s = {.kind = BOX, .handle = &%s};" t handle);
        Hashtbl.add store ty t;
        t
    | TNamedTup _ -> failwith "unreachable"
  in
  getty

let string_of_program width with_prelude { toplevels; body; ty; fresh } =
  let ident = c_ident_generator fresh in
  let topdecls = ref [] in
  let add_topdecl s = topdecls := s :: !topdecls in
  let get_tag = c_get_tag_generator fresh add_topdecl in
  let pp_program f =
    let cgen = { fresh; ident; get_tag; f } in
    let entry = ident "entry" in
    let fns = toplevels @ [ { name = entry; params = []; body; ret = ty } ] in
    let open Format in
    fprintf f "@[<v 0>";
    List.iter (fun d -> fprintf f "%s@," d) !topdecls;
    List.iter
      (fun s ->
        pp_fn cgen s;
        fprintf f "@,")
      fns;
    fprintf f "int main() { _print(%s()); }@]" entry
  in
  let prog = with_buffer pp_program width in
  let prog = String.concat "\n" (List.rev (prog :: !topdecls)) in
  if with_prelude then prelude ^ "\n" ^ prog else prog
