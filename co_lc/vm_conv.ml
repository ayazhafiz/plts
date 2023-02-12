(** Translation to the compiler VM.

    Stack representation of tuples (data):

      ...
      t.last
      ...
      t.0
      === < stack top

    Stack representation of calls:

      return_value
      arg
      <captures>
      fn_name
      --- < stack top on call
      old_pc
      old_fp
      old_sp
      --- < new frame pointer = new stack pointer on enter
*)

open Vm_layout
module T = Ty_solve.T

module Ctx = struct
  open Vm_op

  type name = [ `FpOffset of int | `Proc of label ]

  type t = {
    new_label : string -> label;
    (* proc -> basic blocks *)
    program :
      ( label,
        (label * op list ref) list ref * Vm_debug.debug_frame ref )
      Hashtbl.t;
    (* stack of procs, basic blocks, and locals of frames we're currently compiling.
       the top of the stack is the procedure currently being compiled. *)
    mutable procs_stack : label list;
    mutable bbs_stack : op list ref list;
    (* the current frame pointer offset for the next local *)
    mutable fp_offsets_stack : int ref list;
    (* names are represented as
         (name, (depth, kind))
       kind can be either a local or a proc.

       Locals are only accessible at the current depth.
       Procs are accessible from any lower depth.

       This way we can handle shadowing without any prior alpha-conversion pass.
    *)
    mutable names :
      (string * (int * Ast.ty * [ `FpOffset of int | `Proc of label ])) list;
  }

  let new_ctx () =
    {
      new_label =
        (let fresh_name = Util.fresh_name_generator () in
         fun hint -> `Label (fresh_name hint));
      program = Hashtbl.create ~random:false 128;
      procs_stack = [];
      bbs_stack = [];
      fp_offsets_stack = [];
      names = [];
    }

  let new_label c s = c.new_label s
  let current_depth c = List.length c.procs_stack
  let current_fp_offset c = List.hd c.fp_offsets_stack
  let current_proc c = List.hd c.procs_stack
  let current_bb c = List.hd c.bbs_stack

  let add_local c x ty =
    let offset = current_fp_offset c in
    let depth = current_depth c in
    let target = `FpOffset !offset in
    let stksize = stack_size ty in
    offset := !offset + stksize;
    c.names <- (x, (depth, ty, target)) :: c.names;
    target

  let add_proc_name c x t_proc proc =
    let depth = current_depth c in
    c.names <- (x, (depth, t_proc, `Proc proc)) :: c.names

  let lookup c x : name =
    let current_depth = current_depth c in
    match List.assoc_opt x c.names with
    | Some (depth, _, `FpOffset n) when current_depth = depth -> `FpOffset n
    | Some (_, _, `Proc l) -> `Proc l
    | Some (_, _, `FpOffset _) ->
        let (`Label proc) = current_proc c in
        failwith
          (Printf.sprintf "%s found in %s, but it's at a lower depth" x proc)
    | None ->
        let (`Label proc) = current_proc c in
        failwith (Printf.sprintf "%s not found in %s" x proc)

  let new_bb c label =
    let proc_bbs, _ = Hashtbl.find c.program (current_proc c) in
    let bb_instrs = ref [] in
    let bb = (label, bb_instrs) in
    proc_bbs := bb :: !proc_bbs;
    (* swap in the new basic block onto the currently-compiling frame state *)
    c.bbs_stack <- bb_instrs :: List.tl c.bbs_stack

  let set_bb c label =
    let proc_bbs, _ = Hashtbl.find c.program (current_proc c) in
    let bb_instrs = List.assoc label !proc_bbs in
    c.bbs_stack <- bb_instrs :: List.tl c.bbs_stack

  let push (c : t) (op : op) =
    let bb = current_bb c in
    bb := op :: !bb

  let extend (c : t) (ops : op list) =
    let bb = current_bb c in
    bb := List.rev ops @ !bb

  (* Synthetic name for a return local, not typable in the surface syntax. *)
  let return_local = "#return"

  (** Creates a new procedure and enters it.
      Returns the target of the return value. *)
  let enter_proc c proc_label ~arg:(arg_ty, arg_name) ~ret =
    assert (not (Hashtbl.mem c.program proc_label));
    (* arrange the entry of the proc as follows:

       proc_name:
         <fixup stack adjustment>
       proc_name_start:
         <main instrs>

       we'll set `proc_name_start` as the initial basic block of the actual
       procedure. Instructions in proc_name only contain adjustments to the
       stack to reserve space for locals, which we'll know how to do on proc
       exit.
    *)
    let entry_bb = (proc_label, ref []) in
    let start_instrs = ref [] in
    let start_bb = (new_label c "start", start_instrs) in
    let debug_frame = ref (Vm_debug.new_debug_frame ()) in
    Hashtbl.add c.program proc_label (ref [ start_bb; entry_bb ], debug_frame);
    (* push on a new level for this proc *)
    c.procs_stack <- proc_label :: c.procs_stack;
    c.bbs_stack <- start_instrs :: c.bbs_stack;
    c.fp_offsets_stack <- ref 0 :: c.fp_offsets_stack;

    (* add the argument and its offset to the locals.
       for a given call, we have the stack layout

       #return_value
       arg
       <captures>
       ---
       old_pc
       old_fp
       old_sp
       --- < new frame pointer = new stack pointer

       so, the arg starts at at fp[-3 - closure_stksize - argstksize]

       TODO: also record the closure arguments
    *)
    let depth = current_depth c in
    let arg_stksize = stack_size arg_ty in
    let arg_offset = -3 - arg_stksize in
    c.names <- (arg_name, (depth, arg_ty, `FpOffset arg_offset)) :: c.names;

    (* add a local for the return value *)
    let ret_stksize = stack_size ret in
    let ret_target = arg_offset - ret_stksize in
    let return_target = `FpOffset ret_target in
    c.names <- (return_local, (depth, ret, return_target)) :: c.names;

    (* TODO: if this is a recursive closures, we also need to store the captures
       somewhere locally *)
    return_target

  let exit_proc c proc_label =
    assert (current_proc c = proc_label);
    (* go back to the start of the procedure and reserve space for all our
       locals *)
    set_bb c proc_label;
    let locals_space = !(current_fp_offset c) in
    push c (Vm_op.SpAdd locals_space);
    (* pop names at the last depth *)
    let depth = current_depth c in
    let rec popper locals = function
      | (x, (d, ty, `FpOffset n)) :: rest when d = depth ->
          popper ((x, (ty, `FpOffset n)) :: locals) rest
      | (x, (d, ty, `Proc n)) :: rest ->
          (* TODO this is an awful hack to make procedures always visible.
             Remove it when closures are supported. *)
          let debug_frame, l = popper locals rest in
          (debug_frame, (x, (d, ty, `Proc n)) :: l)
      | l ->
          let debug_frame : Vm_debug.debug_frame = { locals } in
          (debug_frame, l)
    in
    let found_debug_frame, popped_names = popper [] c.names in
    c.names <- popped_names;
    let _, debug_frame = Hashtbl.find c.program (current_proc c) in
    debug_frame := found_debug_frame;
    (* pop off the level used to compile the proc *)
    c.procs_stack <- List.tl c.procs_stack;
    c.bbs_stack <- List.tl c.bbs_stack;
    c.fp_offsets_stack <- List.tl c.fp_offsets_stack

  (* Returns a hashmap of procs to their basic blocks, now immutable and in proper order. *)
  let collapse_into_procs { program; _ } : (label, proc) Hashtbl.t =
    let new_map = Hashtbl.create ~random:false (Hashtbl.length program) in
    let process_proc proc (bbs, debug_frame) =
      let bbs = !bbs in
      let debug_frame = !debug_frame in
      let reified_bbs =
        List.rev @@ List.map (fun (label, bb) -> (label, List.rev !bb)) bbs
      in
      Hashtbl.add new_map proc
        { name = proc; blocks = reified_bbs; debug_frame }
    in
    Hashtbl.iter process_proc program;
    new_map
end

type target =
  [ `FpOffset of int  (** stored into an offset from the frame pointer. *)
  | `Stack  (** stored on the top of the stack. *)
  | `NonCapturingProc of Vm_op.label
    (** the value is a non-capturing proc and can be referenced directly by name. *)
  ]
[@@deriving show]
(** How a value is stored. *)

let target_add n = function
  | `FpOffset m -> `FpOffset (m + n)
  | `Stack -> `Stack (* already on the stack, nowhere to look forward *)

type opt_target =
  [ `Any  (** compiler chooses, returns where value is stored *)
  | `FpOffset of int
  | `Stack ]

let compile_lit ctx l (target : opt_target) =
  let imm = match l with `Bool b -> if b then 1 else 0 | `Int n -> n in
  match target with
  | `Stack | `Any ->
      Ctx.push ctx (Vm_op.Push (`Imm imm));
      `Stack
  | `FpOffset n ->
      Ctx.push ctx (Vm_op.Push (`Imm imm));
      Ctx.push ctx (Vm_op.Store n);
      `FpOffset n

let fiber_return_stack_size t =
  let open Ast in
  match !(unlink t) with
  | Content (TFiber t) -> stack_size t
  | _ -> failwith "non-fiber type"

(** Store data from the ephemeral stack into frame-pointer offsets. *)
let store_into ctx (`FpOffset fp_base) pop_n =
  (*
    x1
    ..
    xn < stack top
    ==>
    popn x1 < fp base
    ..
    pop1 xn < fp top
   *)
  let rec go n =
    assert (n >= 0);
    if n == pop_n then ()
    else (
      (* earlier pops are done first, they store into the higher indices
         pop1 ... popn
      *)
      Ctx.push ctx (Vm_op.Store (fp_base + n));
      go (n + 1))
  in
  go 0

(** Store data from frame-pointer offsets onto the ephemeral stack. *)
let push_from ctx (`FpOffset fp_base) push_n =
  (*
    xn=fp base
    ..
    x1=fp top
    ==>
    push1 xn
    ..
    pushn x1 < stack top
   *)
  let rec go n =
    assert (n >= 0);
    if n == 0 then ()
    else
      (* earlier pushes push from the higher indices
         push1 @ xn ... pushn @ x1
      *)
      let offset = n - 1 in
      Ctx.push ctx (Vm_op.Push (`FpOffset (fp_base + offset)));
      go (n - 1)
  in
  go push_n

let load_name ctx name t target =
  match (name, target) with
  | `FpOffset name, `Any -> `FpOffset name
  | `FpOffset name, `FpOffset other ->
      let stksize = stack_size t in
      (* push name to stack, then load from stack *)
      push_from ctx (`FpOffset name) stksize;
      store_into ctx (`FpOffset other) stksize;
      `FpOffset other
  | `FpOffset name, `Stack ->
      let stksize = stack_size t in
      push_from ctx (`FpOffset name) stksize;
      `Stack
  | `Proc p, (`Any | `Stack) ->
      Ctx.push ctx (Vm_op.Push (Vm_op.locator_of_label p));
      `Stack
  | `Proc p, `FpOffset target ->
      Ctx.push ctx (Vm_op.Push (Vm_op.locator_of_label p));
      store_into ctx (`FpOffset target) 1;
      `FpOffset target

let load_access ctx (target_tup : target) t idx (target_access : opt_target) =
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
  let t_item, item_size, offset_from_top, offset_from_bottom =
    match !(Ast.unlink t) with
    | Content (TTup ts) ->
        let t_item = List.nth ts idx in
        let item_size = stack_size @@ t_item in
        let before = List.filteri (fun i _ -> i < idx) ts in
        let after = List.filteri (fun i _ -> i > idx) ts in
        let offset_from_top =
          List.fold_left ( + ) 0 @@ List.map stack_size @@ before
        in
        let offset_from_bottom =
          List.fold_left ( + ) 0 @@ List.map stack_size @@ after
        in
        (t_item, item_size, offset_from_top, offset_from_bottom)
    | _ -> failwith "not a tuple"
  in
  assert (stksize_rcd = item_size + offset_from_top + offset_from_bottom);
  match (target_tup, target_access) with
  | `FpOffset base, `Any -> `FpOffset (base + offset_from_bottom)
  | `FpOffset base, `FpOffset other ->
      (* push to stack, then load from it *)
      push_from ctx (`FpOffset (base + offset_from_bottom)) item_size;
      store_into ctx (`FpOffset other) item_size;
      `FpOffset other
  | `FpOffset base, `Stack ->
      push_from ctx (`FpOffset (base + offset_from_bottom)) item_size;
      `Stack
  | `Stack, (`Any | `Stack) ->
      (* drop until start is reached *)
      Ctx.push ctx (Vm_op.SpSub offset_from_top);
      (* create a new local for temporarily saving the item *)
      let local = "#access_tmp" in
      let local_target = Ctx.add_local ctx local t_item in
      store_into ctx local_target item_size;
      (* drop all remaining items of the tuple off the stack *)
      Ctx.push ctx (Vm_op.SpSub offset_from_bottom);
      (* push the single wanted item back on *)
      push_from ctx local_target item_size;
      `Stack
  | `Stack, `FpOffset base ->
      (* drop until start is reached, then store *)
      Ctx.push ctx (Vm_op.SpSub offset_from_top);
      store_into ctx (`FpOffset base) item_size;
      (* drop all remaining items of the tuple off the stack *)
      Ctx.push ctx (Vm_op.SpSub offset_from_bottom);
      `FpOffset base
  | `NonCapturingProc _, _ -> failwith "records are not procs"

let store_from_stack ctx target t =
  match target with
  | `Stack -> () (* return is alread on the stack! *)
  | `FpOffset n ->
      (* need to store the return value into a different cell *)
      let ret_size = stack_size t in
      store_into ctx (`FpOffset n) ret_size

let get_content t =
  let open Ast in
  match !(unlink t) with
  | Content c -> c
  | _ -> failwith "not contentful variable during code gen"

let or_stack target =
  match target with (`Stack | `FpOffset _) as n -> n | `Any -> `Stack

let as_opt target : opt_target =
  match target with `Stack -> `Stack | `FpOffset n -> `FpOffset n

let rec compile_proc ctx proc_name (t_x, x) body =
  let t_body = Ast.xty body in
  let return_target = Ctx.enter_proc ctx proc_name ~arg:(t_x, x) ~ret:t_body in
  (* compile the body into the special return local *)
  let _ = compile_expr ctx None body return_target in
  Ctx.push ctx Vm_op.Ret;
  Ctx.exit_proc ctx proc_name

and compile_proc_expr ctx proc_name t_proc (t_x, x) body target =
  compile_proc ctx proc_name (t_x, x) body;
  (* leave behind the proc to call *)
  (* TODO: also leave behind closure data *)
  Ctx.push ctx (Vm_op.Push (Vm_op.locator_of_label proc_name));
  let has_captures = false in
  if has_captures then (
    let target = or_stack target in
    store_from_stack ctx target t_proc;
    target)
  else (
    (* The proc should have been compiled with no expectation of its target,
       because we can reference the label directly. *)
    assert (target = `Any);
    `NonCapturingProc proc_name)

and compile_expr ctx bound_proc e target =
  let rec go ?(bound_proc = None) (_, t, e) (target : opt_target) =
    match e with
    | Ast.Var x ->
        let target_x = Ctx.lookup ctx x in
        load_name ctx target_x t target
    | Ast.Lit l -> compile_lit ctx l target
    | Ast.Tup es ->
        let target = or_stack target in
        let _end_target =
          List.fold_right
            (fun e target ->
              let _ = go e (as_opt target) in
              let target1 = target_add (stack_size (Ast.xty e)) target in
              target1)
            es target
        in
        target
    | Ast.Let (kind, (_, t_x, x), e, rest) ->
        let target_x, bound_proc =
          match get_content t_x with
          | Ast.TFn _ ->
              let has_captures = false in
              let target =
                if not has_captures then
                  (* This is a non-capturing proc; we don't need to allocate a
                     local for it, because it can be passed around by-name. *)
                  `Any
                else failwith "TODO"
              in
              let proc_name = Ctx.new_label ctx x in
              (* If this binding is recursive, associate the recursive proc name now.
                 TODO: handle case of recursive, capturing procs. *)
              if kind = `Rec then Ctx.add_proc_name ctx x t_x proc_name;
              (target, Some proc_name)
          | _ ->
              (* non-proc must always be stored locally *)
              (Ctx.add_local ctx x t_x, None)
        in
        let stored_x = go ~bound_proc e target_x in
        (* If we compiled a binding to a non-capturing proc, be sure to
           associate the binding name to that proc, without any additional
           storage. *)
        (match stored_x with
        | `NonCapturingProc name ->
            assert (Some name = bound_proc);
            Ctx.add_proc_name ctx x t_x name
        | _ -> ());
        (* Build the rest of the program following the binding. *)
        let end_target = go rest target in
        end_target
    | Ast.Abs ((_, t_x, x), e) ->
        let proc_name =
          match bound_proc with
          | Some proc -> proc
          | None -> Ctx.new_label ctx "proc"
        in
        compile_proc_expr ctx proc_name t (t_x, x) e target
    | Ast.App (fn, arg) ->
        (* call:
           #return
           arg
           <captures>
           fn_label < stack top
        *)
        (* TODO support captures *)
        let t_fn = Ast.xty fn in
        let fn_stksize = stack_size t_fn in
        assert (fn_stksize = 1);
        let t_ret =
          match get_content t_fn with
          | Ast.TFn (_, ret) -> ret
          | _ -> failwith "not a function"
        in
        (* Allocate space for the return value *)
        Ctx.push ctx (Vm_op.SpAdd (stack_size t_ret));
        (* Push the arg, and function, then call *)
        let _ = go arg `Stack in
        let _ = go fn `Stack in
        Ctx.push ctx Vm_op.Call;
        (* After the call, rollback whatever space we allocated for the captures
           and arguments, so that the return value is on the top of the stack.

           -1 for the proc label, which was already popped during the call.
        *)
        let allocated_arg_space = stack_size (Ast.xty arg) + fn_stksize - 1 in
        Ctx.push ctx (Vm_op.SpSub allocated_arg_space);
        (* Store the return value into the target. *)
        let ret_target = or_stack target in
        store_from_stack ctx ret_target t;
        ret_target
    | Ast.Binop (op, arg1, arg2) ->
        let _ = go arg2 `Stack in
        let _ = go arg1 `Stack in
        let call =
          match op with
          | `Lt -> Vm_op.Lt
          | `Add -> Vm_op.Add
          | `Sub -> Vm_op.Sub
          | `Mul -> Vm_op.Mul
        in
        Ctx.push ctx call;
        let ret_target = or_stack target in
        store_from_stack ctx ret_target t;
        ret_target
    | Ast.If (cond, then', else') ->
        let lbl_then = Ctx.new_label ctx "then" in
        let lbl_else = Ctx.new_label ctx "else" in
        let lbl_join = Ctx.new_label ctx "join" in
        (* Build the condition. If it's false (0) jump to the else branch. *)
        let _ = go cond `Stack in
        Ctx.push ctx (Vm_op.Jmpz lbl_else);
        (* The overall result must join into the same location. *)
        let join_target = or_stack target in
        (* then branch *)
        Ctx.new_bb ctx lbl_then;
        let then_target = go then' join_target in
        Ctx.push ctx (Vm_op.Jmp lbl_join);
        (* else branch *)
        Ctx.new_bb ctx lbl_else;
        let else_target = go else' join_target in
        Ctx.push ctx (Vm_op.Jmp lbl_join);
        (* the rest of the procedure starts at the join *)
        Ctx.new_bb ctx lbl_join;
        assert (then_target = else_target);
        then_target
    | Ast.Access (rcd, idx) ->
        let target_tup = go rcd `Any in
        load_access ctx target_tup (Ast.xty rcd) idx target
    | Ast.Spawn body ->
        (* Convert
             spawn e
           to
             proc = \{} -> e
             spawn proc
        *)
        let proc_name = Ctx.new_label ctx "spawn_wrapper" in
        let t_body = Ast.xty body in
        let t_spawn_wrapper = ref @@ Ast.Content (Ast.TFn (T.unit, t_body)) in
        let arg = (T.unit, "") in
        let _ =
          compile_proc_expr ctx proc_name t_spawn_wrapper arg body `Stack
        in
        (* Call spawn, then store the returned fiber into the target. *)
        let args_size = 0 (* TODO material once closures are supported *) in
        let ret_size = stack_size t_body in
        Ctx.push ctx (Vm_op.Spawn { args_size; ret_size });
        let fiber_target = or_stack target in
        let t_fiber = ref @@ Ast.Content (Ast.TFiber t_body) in
        store_from_stack ctx fiber_target t_fiber;
        fiber_target
    | Ast.Yield ->
        Ctx.push ctx Vm_op.Yield;
        `Stack (* yield is zero-sized *)
    | Ast.Resume e ->
        let _ = go e `Stack in
        Ctx.push ctx (Vm_op.Resume (fiber_return_stack_size t));
        let ret_target = or_stack target in
        store_from_stack ctx ret_target t;
        ret_target
    | Ast.Stat { cond; pending; done' = (_, t_x, x), done_body } ->
        let lbl_pending = Ctx.new_label ctx "pending" in
        let lbl_done = Ctx.new_label ctx "done" in
        let lbl_join = Ctx.new_label ctx "join" in
        (* Load fiber onto the stack:

           stkdirty
           stkidx
           return_value
           bit          < stack top

           bit=0 if Pending
           bit=1 if Done
        *)
        let _ = go cond `Stack in
        Ctx.push ctx (Vm_op.Jmpz lbl_pending);
        (* The overall result must join into the same location. *)
        let join_target = or_stack target in
        (* Done branch - load the return value into x, then evaluate the body.
           The return value is on the top of the stack now, so can be loaded as a
           return. *)
        Ctx.new_bb ctx lbl_done;
        let target_x = Ctx.add_local ctx x t_x in
        store_from_stack ctx target_x t_x;
        let done_target = go done_body join_target in
        Ctx.push ctx (Vm_op.Jmp lbl_join);
        (* Pending branch *)
        Ctx.new_bb ctx lbl_pending;
        let pending_target = go pending join_target in
        Ctx.push ctx (Vm_op.Jmp lbl_join);
        (* the rest of the procedure starts at the join *)
        Ctx.new_bb ctx lbl_join;
        assert (done_target = pending_target);
        done_target
  in
  go ?bound_proc e target

let compile : Ast.program -> Vm_op.program =
 fun program ->
  let open Vm_op in
  let ctx = Ctx.new_ctx () in
  let ret_ty = Ast.xty program in
  let ret_size = stack_size ret_ty in
  compile_proc ctx main (T.unit, "") program;
  let procs = Ctx.collapse_into_procs ctx in
  let main_proc = Hashtbl.find procs main in
  Hashtbl.remove procs main;
  let procs = (List.of_seq @@ Hashtbl.to_seq_values procs) @ [ main_proc ] in
  { procs; ret_size; ret_ty }
