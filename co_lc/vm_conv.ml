(** Translation to the compiler VM.

    Stack representation of tuples (data):

      ...
      t.last
      ...
      t.0
      === < stack top

    Stack representation of calls:

      ...
      arg_last
      ...
      arg_1
      fn_name
      === < stack top
*)

module Target = struct
  type t = [ `Stack | `FpOffset of int ]

  type locals = {
    offset : int ref;
    names : (string * [ `FpOffset of int ]) list ref;
  }

  let new_locals () = { offset = ref 0; names = ref [] }

  (** Advance a target on the stack or offset from frame pointer. *)
  let add t n =
    match t with `Stack -> `Stack | `FpOffset m -> `FpOffset (m + n)

  let add_local { offset; names } x stksize =
    let target = `FpOffset !offset in
    offset := !offset + stksize;
    names := (x, target) :: !names;
    target

  let lookup { names; _ } x = List.assoc x !names
end

type ctx = { new_label : string -> Vm_op.label }

type opt_target =
  [ `Any  (** compiler chooses, returns where value is stored *) | Target.t ]

let compile_lit l (target : opt_target) =
  let imm = match l with `Bool b -> if b then 1 else 0 | `Int n -> n in
  match target with
  | `Stack | `Any -> (`Stack, [ Vm_op.Push (`Imm imm) ])
  | `FpOffset n -> (`FpOffset n, [ Vm_op.Push (`Imm imm); Vm_op.Store n ])

(** Stack size in integer cells. *)
let rec stack_size t =
  let open Ast in
  match !(unlink t) with
  | Link _ | Unbd _ -> failwith "non-contentful type"
  | Content c -> (
      match c with
      | TInt -> 1
      | TBool -> 1
      | TTup ts -> List.fold_left ( + ) 0 @@ List.map stack_size ts
      | TTupSparse _ -> failwith "non-concrete tuple type"
      | TFn _ ->
          (* 1 word for the function name *)
          (* TODO: closure data *)
          1
      | TFiber t ->
          (* {bit, return_value, stkidx, stkdirty}
              1    t             1       1
          *)
          1 + stack_size t + 1 + 1)

(** Store data from the ephemeral stack into frame-pointer offsets. *)
let store_into (`FpOffset fp_base) pop_n =
  (*
    x1
    ..
    xn < stack top
    ==>
    popn x1 < fp base
    ..
    pop1 xn < fp top
   *)
  let rec go instrs n =
    assert (n >= 0);
    if n == pop_n then instrs
    else
      (* earlier pops are done first, they store into the higher indices
         pop1 ... popn
      *)
      let store = Vm_op.Store (fp_base + n) in
      go (store :: instrs) (n + 1)
  in
  go [] 0

(** Store data from frame-pointer offsets onto the ephemeral stack. *)
let push_from (`FpOffset fp_base) push_n =
  (*
    xn=fp base
    ..
    x1=fp top
    ==>
    push1 xn
    ..
    pushn x1 < stack top
   *)
  let rec go instrs n =
    assert (n >= 0);
    if n == 0 then instrs
    else
      (* earlier pushes push from the higher indices
         push1 @ xn ... pushn @ x1
      *)
      let offset = n - 1 in
      let push = Vm_op.Push (`FpOffset (fp_base + offset)) in
      go (push :: instrs) (n - 1)
  in
  go [] push_n

let rec load_name (`FpOffset name) t = function
  | `Any -> (`FpOffset name, [])
  | `FpOffset other ->
      let stksize = stack_size t in
      (* push name to stack, then load from stack *)
      let instrs =
        push_from (`FpOffset name) stksize
        @ store_into (`FpOffset other) stksize
      in
      (`FpOffset other, instrs)
  | `Stack ->
      let stksize = stack_size t in
      (`Stack, push_from (`FpOffset name) stksize)

let load_access (target_rcd : Target.t) t idx (target_access : opt_target) =
  (*
    |     | .n < bottom
    |     | ..
    |   | | .item_end
    |   |   ..
    | | |   .item_start
    | |     ..
    | |     .0 < stack top
          |-offset_from_bottom
        |-item_size
      |-offset_from_top
    |-stksize_rcd
   *)
  let stksize_rcd = stack_size t in
  let item_size, offset_from_top, offset_from_bottom =
    match !(Ast.unlink t) with
    | Content (TTup ts) ->
        let item_size = stack_size @@ List.nth ts idx in
        let before = List.filteri (fun i _ -> i < idx) ts in
        let after = List.filteri (fun i _ -> i < idx) ts in
        let offset_from_top =
          List.fold_left ( + ) 0 @@ List.map stack_size @@ before
        in
        let offset_from_bottom =
          List.fold_left ( + ) 0 @@ List.map stack_size @@ after
        in
        (item_size, offset_from_top, offset_from_bottom)
    | _ -> failwith "not a tuple"
  in
  assert (stksize_rcd = item_size + offset_from_top + offset_from_bottom);
  match (target_rcd, target_access) with
  | `FpOffset base, `Any -> (`FpOffset (base + offset_from_bottom), [])
  | `FpOffset base, `FpOffset other ->
      (* push to stack, then load from it *)
      let instrs =
        push_from (`FpOffset (base + offset_from_bottom)) item_size
        @ store_into (`FpOffset other) item_size
      in
      (`FpOffset other, instrs)
  | `FpOffset base, `Stack ->
      (`Stack, push_from (`FpOffset (base + offset_from_bottom)) item_size)
  | `Stack, (`Any | `Stack) ->
      (* drop until start is reached *)
      let instrs = List.init offset_from_top (fun _ -> Vm_op.Drop) in
      (`Stack, instrs)
  | `Stack, `FpOffset base ->
      (* drop until start is reached, then store *)
      let instrs =
        List.init offset_from_top (fun _ -> Vm_op.Drop)
        @ store_into (`FpOffset base) item_size
      in
      (`FpOffset base, instrs)

let store_call target t =
  match target with
  | `Stack -> [] (* return is alread on the stack! *)
  | `FpOffset n ->
      (* need to store the return value into a different cell *)
      let ret_size = stack_size t in
      store_into (`FpOffset n) ret_size

let or_stack (target : opt_target) =
  match target with (`Stack | `FpOffset _) as n -> n | `Any -> `Stack

let as_opt (target : Target.t) : opt_target =
  match target with `Stack -> `Stack | `FpOffset n -> `FpOffset n

let rec compile_expr e ctx names target =
  let rec go (_, t, e) (target : [ `Any | Target.t ]) =
    match e with
    | Ast.Var x ->
        let target_x = Target.lookup names x in
        load_name target_x t target
    | Ast.Lit l -> compile_lit l target
    | Ast.Tup es ->
        let target = or_stack target in
        let end_target, instrs =
          List.fold_right
            (fun e (target, instrs) ->
              let _, instrs1 = go e (as_opt target) in
              let target1 = Target.add target (stack_size (Ast.xty e)) in
              (target1, instrs @ instrs1))
            es (target, [])
        in
        (end_target, instrs)
    | Ast.Let (_, (_, t_x, x), e, rest) ->
        let target_x = Target.add_local names x (stack_size t_x) in
        let _, instrs_x = go e target_x in
        let end_target, instrs_rest = go rest target in
        (end_target, instrs_x @ instrs_rest)
    | Ast.Abs (_, _) -> failwith "todo"
    | Ast.App (fn, arg) ->
        (* call: fn args, fn < stack top *)
        assert (stack_size (Ast.xty fn) = 1);
        (* -1 for function label *)
        let arg_size = stack_size (Ast.xty arg) + stack_size (Ast.xty fn) - 1 in
        let _, iarg = go arg `Stack in
        let _, ifn = go fn `Stack in
        let icall = [ Vm_op.Call arg_size ] in
        let ret_target = or_stack target in
        let istores = store_call ret_target t in
        (ret_target, iarg @ ifn @ icall @ istores)
    | Ast.Binop (op, arg1, arg2) ->
        let _, iarg1 = go arg1 `Stack in
        let _, iarg2 = go arg2 `Stack in
        let call =
          match op with
          | `Lt -> Vm_op.Lt
          | `Add -> Vm_op.Add
          | `Sub -> Vm_op.Sub
        in
        let ret_target = or_stack target in
        let istores = store_call ret_target t in
        (ret_target, iarg1 @ iarg2 @ [ call ] @ istores)
    | Ast.If (cond, then', else') ->
        let _, icond = go cond `Stack in
        let lbl_then = ctx.new_label "then" in
        let _, ithen = go then' target in
        let ithen = (lbl_then, ithen) in
        let lbl_else = ctx.new_label "else" in
        let _, ielse = go else' target in
        let else' = (lbl_then, ithen) in
        let ijmp = [ (Vm_op.Push (`Imm 1), Vm_op.Eq, Vm_op.Jmpz lbl_else) ] in
        failwith "todo"
    | Ast.Access (rcd, idx) ->
        let target_rcd, ircd = go rcd `Any in
        load_access target_rcd (Ast.xty rcd) idx target
    | Ast.Spawn _ -> _
    | Ast.Yield -> _
    | Ast.Resume _ -> _
    | Ast.Stat _ -> _
  in
  go e target

let compile : Ast.program -> Vm_op.program = fun program -> compile_expr program
