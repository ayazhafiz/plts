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
      <captures.last>
      ...
      <captures.0>
      fn_name
      --- < stack top on call
      old_pc
      old_fp
      old_sp
      --- < new frame pointer = new stack pointer on enter
*)

open Vm_layout
open Symbol
module T = Ty_solve.T

module Ctx = struct
  open Vm_op

  type name = [ `FpOffset of int ]

  type t = {
    new_label : string -> label;
    (* proc -> basic blocks *)
    program :
      ( label,
        (label * op list ref) list ref * Vm_debug.debug_frame ref )
      Hashtbl.t;
    (* stack of procs, basic blocks, and locals of frames we're currently compiling.
       the top of the stack is the procedure currently being compiled. *)
    mutable procs_stack : (label * symbol) list;
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
    mutable names : (symbol * (int * Ast.ty * [ `FpOffset of int ])) list;
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
  let label_of_sym (`Sym x) = `Label x
  let sym_of_label (`Label x) = `Sym x
  let current_depth c = List.length c.procs_stack
  let current_fp_offset c = List.hd c.fp_offsets_stack
  let current_proc c = fst @@ List.hd c.procs_stack
  let current_bb c = List.hd c.bbs_stack

  let add_local c x ty =
    let offset = current_fp_offset c in
    let depth = current_depth c in
    let target = `FpOffset !offset in
    let stksize = stack_size ty in
    offset := !offset + stksize;
    c.names <- (x, (depth, ty, target)) :: c.names;
    target

  type reserved = { target : [ `FpOffset of int ]; ty : Ast.ty }

  let reserve_local c ty =
    let offset = current_fp_offset c in
    let target = `FpOffset !offset in
    let stksize = stack_size ty in
    offset := !offset + stksize;
    { target; ty }

  let add_reserved_local c x { target = `FpOffset n; ty } =
    let depth = current_depth c in
    c.names <- (x, (depth, ty, `FpOffset n)) :: c.names

  let lookup_help c x =
    let current_depth = current_depth c in
    match List.assoc_opt x c.names with
    | Some (depth, _, `FpOffset n) when current_depth = depth ->
        `Found (`FpOffset n)
    | Some (_, _, `FpOffset _) ->
        let (`Label proc) = current_proc c in
        let (`Sym x) = x in
        `FoundButLowerDepth (x, proc)
    | None ->
        let (`Label proc) = current_proc c in
        let (`Sym x) = x in
        `NotFound (x, proc)

  let lookup c x =
    match lookup_help c x with
    | `Found off -> off
    | `FoundButLowerDepth (x, proc) ->
        failwith
          (Printf.sprintf "%s found in %s, but it's at a lower depth" x proc)
    | `NotFound (x, proc) ->
        failwith (Printf.sprintf "%s not found in %s" x proc)

  let lookup_opt c x =
    match lookup_help c x with
    | `Found off -> Some off
    | `FoundButLowerDepth (x, proc) ->
        failwith
          (Printf.sprintf "%s found in %s, but it's at a lower depth" x proc)
    | `NotFound _ -> None

  let current_proc_argument c = lookup c @@ snd @@ List.hd c.procs_stack

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
  let return_local = `Sym "#return"

  (** Creates a new procedure and enters it.
      Returns the target of the return value. *)
  let enter_proc c proc_label ~is_rec t_fn ~arg:(arg_ty, arg_name)
      ~captures:(captures, closure_stksize) ~ret =
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
    c.procs_stack <- (proc_label, arg_name) :: c.procs_stack;
    c.bbs_stack <- start_instrs :: c.bbs_stack;
    c.fp_offsets_stack <- ref 0 :: c.fp_offsets_stack;

    (* add the argument and its offset to the locals.
       for a given call, we have the stack layout

       #return_value
       arg
       <captures.last>
       ...
       <captures.0>
       ---
       old_pc
       old_fp
       old_sp
       --- < new frame pointer = new stack pointer

       so, the arg starts at at fp[-3 - closure_stksize - argstksize]
    *)
    let depth = current_depth c in
    let arg_stksize = stack_size arg_ty in
    let captures_offset = -3 - closure_stksize in

    let _off =
      List.fold_left
        (fun off (x, t) ->
          c.names <- (x, (depth, t, `FpOffset off)) :: c.names;
          off + stack_size t)
        captures_offset
      @@ List.rev captures
    in

    let arg_offset = -3 - closure_stksize - arg_stksize in
    c.names <- (arg_name, (depth, arg_ty, `FpOffset arg_offset)) :: c.names;

    (* add a local for the return value *)
    let ret_stksize = stack_size ret in
    let ret_target = arg_offset - ret_stksize in
    let return_target = `FpOffset ret_target in
    c.names <- (return_local, (depth, ret, return_target)) :: c.names;

    (* If this is a recursive closure, bind the name to the captures now too *)
    if is_rec && closure_stksize <> 0 then
      c.names <-
        (sym_of_label proc_label, (depth, t_fn, `FpOffset captures_offset))
        :: c.names;

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

  let return_target ctx = lookup ctx return_local

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
[@@deriving show]

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
    if n == 0 then ()
    else
      (* earlier pops are done first, they store into the higher indices
         pop1 ... popn
      *)
      let offset = n - 1 in
      Ctx.push ctx (Vm_op.Store (fp_base + offset));
      go (n - 1)
  in
  go pop_n

(** Store data from frame-pointer offsets onto the ephemeral stack. *)
let push_from ctx (`FpOffset fp_base) push_n =
  (*
    x1=fp base
    ..
    xn=fp top
    ==>
    push1 x1
    ..
    pushn xn < stack top
   *)
  let rec go n =
    assert (n >= 0);
    if n == push_n then ()
    else (
      (* earlier pushes push from the higher indices
         push1 @ xn ... pushn @ x1
      *)
      Ctx.push ctx (Vm_op.Push (`FpOffset (fp_base + n)));
      go (n + 1))
  in
  go 0

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
      let local = `Sym "#access_tmp" in
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

let store_from_stack_sized ctx target size =
  match target with
  | `Stack -> () (* return is alread on the stack! *)
  | `FpOffset n ->
      (* need to store the return value into a different cell *)
      store_into ctx (`FpOffset n) size

let get_content t =
  let open Ast in
  match !(unlink t) with
  | Content c -> c
  | _ -> failwith "not contentful variable during code gen"

let or_stack target =
  match target with (`Stack | `FpOffset _) as n -> n | `Any -> `Stack

let as_opt target : opt_target =
  match target with `Stack -> `Stack | `FpOffset n -> `FpOffset n

let rec compile_proc ctx ~is_rec proc_name t_proc (t_x, x) ~captures body =
  let t_body = Ast.xty body in
  let return_target =
    Ctx.enter_proc ctx proc_name ~is_rec t_proc ~arg:(t_x, x) ~ret:t_body
      ~captures
  in
  (* compile the body into the special return local *)
  let _ = compile_expr ctx body return_target in
  Ctx.push ctx Vm_op.Ret;
  Ctx.exit_proc ctx proc_name

and compile_proc_expr ctx proc_name ~is_rec t_proc (t_x, x) lambda_set body
    target =
  let { tag; captures; captures_stksize } =
    callee_set_storage (Ctx.sym_of_label proc_name) lambda_set
  in
  compile_proc ctx proc_name ~is_rec t_proc (t_x, x)
    ~captures:(captures, captures_stksize)
    body;
  (* leave behind the proc to call *)
  let has_captures = captures_stksize > 0 in
  match (has_captures, target) with
  | false, `Any when Option.is_none tag ->
      (* Return the proc reference directly *)
      `NonCapturingProc proc_name
  | false, _ ->
      let target = or_stack target in
      (match tag with
      | Some t ->
          Ctx.push ctx (Vm_op.Push (`Imm t));
          store_from_stack_sized ctx target 1
      | None -> assert (stack_size t_proc = 0));
      target
  | true, target ->
      (* load the captures and symbol into the target *)
      let closure_target = or_stack target in
      let captures = List.rev captures in
      let target =
        List.fold_left
          (fun target (x, t) ->
            let target_x = Ctx.lookup ctx x in
            let _ = load_name ctx target_x t target in
            target_add (stack_size t) target)
          closure_target captures
      in

      let loaded_size =
        List.fold_left (fun n (_, t) -> n + stack_size t) 0 captures
      in

      let needed_padding = captures_stksize - loaded_size in
      let padding_lits = List.init needed_padding (fun _ -> `Int 0) in
      let target =
        List.fold_left
          (fun target lit ->
            let _ = compile_lit ctx lit (as_opt target) in
            target_add 1 target)
          target padding_lits
      in

      (* Store the lambda tag as neeed *)
      (match tag with
      | Some t ->
          Ctx.push ctx (Vm_op.Push (`Imm t));
          store_from_stack_sized ctx target 1
      | None -> ());

      closure_target

and compile_expr ctx e target =
  let rec go (_, t, e) (target : opt_target) =
    match e with
    | Ast.Var x -> (
        match Ctx.lookup_opt ctx x with
        | Some target_x -> load_name ctx target_x t target
        | None -> (
            (* this must be a non-capturing proc *)
            match get_content t with
            | Ast.TFn _ ->
                assert (stack_size t = 0);
                `Stack
            | _ -> assert false))
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
    | Ast.Let (_, (_, t_x, x), e, rest) ->
        let target_x, reserved_x =
          match get_content t_x with
          | Ast.TFn _ when stack_size t_x = 0 ->
              (* This is a non-capturing proc; we don't need to allocate a
                 local for it, because it will be passed around by-name. *)
              (`Any, None)
          | _ ->
              (* non-proc or closure must always be stored locally *)
              let reserved_local = Ctx.reserve_local ctx t_x in
              let target =
                match reserved_local.target with `FpOffset n -> `FpOffset n
              in
              (target, Some reserved_local)
        in
        let stored_x = go e target_x in
        (match stored_x with
        | `NonCapturingProc _ ->
            (* If we compiled a binding to a non-capturing proc, be sure to
               associate the binding name to that proc, without any additional
               storage. *)
            assert (Option.is_none reserved_x)
        | `FpOffset _ ->
            (* Fill in the reserved slot with the actual name now that we're
               going to compile the rest of the program. *)
            let reserved_x = Option.get reserved_x in
            Ctx.add_reserved_local ctx x reserved_x
        | `Stack -> failwith "named binding cannot be stored on stack");
        (* Build the rest of the program following the binding. *)
        let end_target = go rest target in
        end_target
    | Ast.Abs ((lam, is_rec), (_, t_x, x), e) ->
        let proc = Ctx.label_of_sym lam in
        let lambda_set =
          match get_content t with
          | Ast.TFn (_, lambda_set, _) -> lambda_set
          | _ -> failwith "not a function"
        in
        compile_proc_expr ctx proc ~is_rec:!is_rec t (t_x, x) lambda_set e
          target
    | Ast.App (fn, arg) ->
        (* call:
           #return
           arg
           <captures>
           fn_label < stack top
        *)
        let t_fn = Ast.xty fn in
        let fn_stksize = stack_size t_fn in
        let t_ret, lambda_set =
          match get_content t_fn with
          | Ast.TFn (_, lambda_set, ret) -> (ret, lambda_set)
          | _ -> failwith "not a function"
        in

        let perform_tco =
          target = Ctx.return_target ctx
          && List.length lambda_set = 1
          &&
          let target_proc = Ctx.label_of_sym @@ fst @@ List.hd lambda_set in
          target_proc = Ctx.current_proc ctx
        in
        if perform_tco then (
          (* TCO call.
             NB: I believe the target function is immaterial, and does not need
             to be re-evaluated here. The captures also cannot change. The only
             place where this "goes wrong" is if the function expression resumes
             a fiber, but that means that the resumed fiber is never reachable.
             That does break user semantics, though.

             So, we only need to
             - load the argument into its position in the frame
             - rollback the stack pointer to the start of the frame
             - jump to start of frame
          *)
          let arg_target = Ctx.current_proc_argument ctx in
          let _ = go arg arg_target in
          Ctx.push ctx Vm_op.SpRestoreFp;
          Ctx.push ctx (Vm_op.Jmp (Ctx.current_proc ctx));
          Ctx.return_target ctx)
        else (
          (* Non-TCO call *)

          (* Allocate space for the return value *)
          Ctx.push ctx (Vm_op.SpAdd (stack_size t_ret));
          (* Push the arg, and any closure data, then call *)
          let _ = go arg `Stack in
          let _ = go fn `Stack in

          assert (List.length lambda_set > 0);

          (* If the lambda set is non-unary, we need to build a jump table for
             dispatching. *)
          let consumed_lambda_tag_bit =
            match lambda_set with
            | [] -> assert false
            | [ proc ] ->
                (* Unary lambda set can dispatch immediately *)
                let fn = Ctx.label_of_sym @@ fst proc in

                Ctx.push ctx (Vm_op.Call fn);
                0
            | lams ->
                (* More than one lambda; pull down the tag and build a jump table
                   relative to the tag.
                   The tag must already be at the top of the stack so immediately
                   call jumprel. *)

                (* Multiply the tag by 2, since that's the size of each jump block. *)
                Ctx.push ctx (Vm_op.Push (`Imm 2));
                Ctx.push ctx Vm_op.Mul;
                (* Jump relative, and perform the appropriate call. *)
                Ctx.push ctx Vm_op.Jmprel1;

                (* Build each jump label *)
                let lbl_join = Ctx.new_label ctx "join_call" in
                List.iteri
                  (fun i (proc, _) ->
                    let lbl =
                      Ctx.new_label ctx ("call_" ^ string_of_int i ^ "_")
                    in
                    let fn = Ctx.label_of_sym proc in
                    Ctx.new_bb ctx lbl;

                    Ctx.push ctx (Vm_op.Call fn);
                    Ctx.push ctx (Vm_op.Jmp lbl_join))
                  lams;
                (* the rest of the procedure starts at the join *)
                Ctx.new_bb ctx lbl_join;

                1
          in

          (* After the call, rollback whatever space we allocated for the captures
             and arguments, so that the return value is on the top of the stack.

             If the lambda set was non-unary, we also already consumed one bit for
             dispatching.
          *)
          let allocated_arg_space = stack_size (Ast.xty arg) + fn_stksize in
          Ctx.push ctx
            (Vm_op.SpSub (allocated_arg_space - consumed_lambda_tag_bit));
          (* Store the return value into the target. *)
          let ret_target = or_stack target in
          store_from_stack ctx ret_target t;
          ret_target)
    | Ast.Binop (op, arg1, arg2) ->
        let _ = go arg2 `Stack in
        let _ = go arg1 `Stack in
        let call =
          match op with
          | `Lt -> Vm_op.Lt
          | `Eq -> Vm_op.Eq
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
    | Ast.Spawn (body, captures) ->
        (* Convert
             spawn e
           to
             proc = \{} -> e
             spawn proc
        *)
        let proc_name = Ctx.new_label ctx "spawn_wrapper" in
        let t_body = Ast.xty body in
        let captures = !captures in
        let lambda_set = [ (Ctx.sym_of_label proc_name, captures) ] in
        let t_spawn_wrapper =
          ref @@ Ast.Content (Ast.TFn (T.unit, lambda_set, t_body))
        in
        let arg = (T.unit, `Sym "") in
        let _ =
          compile_proc_expr ctx proc_name ~is_rec:false t_spawn_wrapper arg
            lambda_set body `Stack
        in
        (* Call spawn, then store the returned fiber into the target. *)
        let args_size = captures_stksize captures in
        let ret_size = stack_size t_body in
        Ctx.push ctx (Vm_op.Spawn { proc = proc_name; args_size; ret_size });
        let fiber_target = or_stack target in
        let t_fiber = ref @@ Ast.Content (Ast.TFiber t_body) in
        store_from_stack ctx fiber_target t_fiber;
        fiber_target
    | Ast.Yield ->
        Ctx.push ctx Vm_op.Yield;
        or_stack target (* yield is zero-sized *)
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
        (* Done branch - load the return value into x, drop the remaining bits.
           Then evaluate the body.
           The return value is on the top of the stack now, so can be loaded as a
           return. *)
        let done_target =
          Ctx.new_bb ctx lbl_done;
          let target_x = Ctx.add_local ctx x t_x in
          store_from_stack ctx target_x t_x;
          Ctx.push ctx (Vm_op.SpSub 2) (* drop idx, dirty *);
          let done_target = go done_body join_target in
          Ctx.push ctx (Vm_op.Jmp lbl_join);
          done_target
        in
        (* Pending branch *)
        let pending_target =
          Ctx.new_bb ctx lbl_pending;
          (* drop ret, idx, dirty *)
          Ctx.push ctx (Vm_op.SpSub (2 + stack_size t_x));
          let pending_target = go pending join_target in
          Ctx.push ctx (Vm_op.Jmp lbl_join);
          pending_target
        in
        (* the rest of the procedure starts at the join *)
        Ctx.new_bb ctx lbl_join;
        assert (done_target = pending_target);
        done_target
  in
  go e target

let compile : Ast.program -> Vm_op.program =
 fun program ->
  let open Vm_op in
  let ctx = Ctx.new_ctx () in
  let ret_ty = Ast.xty program in
  let ret_size = stack_size ret_ty in
  let captures = ([], 0) in
  let t_main =
    ref
    @@ Ast.Content (Ast.TFn (T.unit, [ (Ctx.sym_of_label main, []) ], ret_ty))
  in
  compile_proc ctx main ~is_rec:false t_main (T.unit, `Sym "") ~captures program;
  let procs = Ctx.collapse_into_procs ctx in
  let main_proc = Hashtbl.find procs main in
  Hashtbl.remove procs main;
  let procs = (List.of_seq @@ Hashtbl.to_seq_values procs) @ [ main_proc ] in
  { procs; ret_size; ret_ty }
