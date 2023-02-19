open Vm_op
module Fiber = Vm_fiber

let bbs_of_procs procs =
  List.flatten @@ List.map (fun { blocks; _ } -> blocks) procs

let build_instruction_table bbs =
  let _, label_tbl =
    List.fold_left
      (fun (n_instr, label_tbl) (label, instrs) ->
        let label_tbl = (label, n_instr) :: label_tbl in
        let next_label_start = n_instr + List.length instrs in
        (next_label_start, label_tbl))
      (0, []) bbs
  in
  let flat_instrs = List.flatten @@ List.map snd bbs in
  (Array.of_list flat_instrs, label_tbl)

type fiber_cell = {
  fiber : Fiber.t;
  dirty : int ref;
  pc : int ref;
  ret_size : int;
}

let eval instrs label_tbl main_fiber main_size =
  let next_idx = ref 1 in
  let next_dirty = ref 1 in
  let i_main = List.assoc main label_tbl in
  let all_fibers =
    let main_cell =
      {
        fiber = main_fiber;
        dirty = ref 0;
        pc = ref i_main;
        ret_size = main_size;
      }
    in
    ref [ (0, main_cell) ]
  in
  let fiber_stack = ref [] in
  (* Setup the initial fiber state to the main fiber. *)
  let fiber_idx = ref 0 in
  let fiber = ref main_fiber in
  let rec go i =
    match Array.get instrs i with
    | Eq ->
        let l = Fiber.pop_int !fiber in
        Fiber.in_place_int !fiber (fun r -> if l = r then 1 else 0);
        go (i + 1)
    | Lt ->
        let l = Fiber.pop_int !fiber in
        Fiber.in_place_int !fiber (fun r -> if l < r then 1 else 0);
        go (i + 1)
    | Sub ->
        let l = Fiber.pop_int !fiber in
        Fiber.in_place_int !fiber (fun r -> l - r);
        go (i + 1)
    | Add ->
        let l = Fiber.pop_int !fiber in
        Fiber.in_place_int !fiber (fun r -> l + r);
        go (i + 1)
    | Mul ->
        let l = Fiber.pop_int !fiber in
        Fiber.in_place_int !fiber (fun r -> l * r);
        go (i + 1)
    | Yield ->
        if !fiber_idx = 0 then
          (* yielding the main fiber does nothing *)
          go (i + 1)
        else
          let child_idx = !fiber_idx in
          (* Pop to the last fiber *)
          let parent_fiber_idx = List.hd !fiber_stack in
          let { fiber = parent_fiber; pc = parent_pc; _ } =
            List.assoc parent_fiber_idx !all_fibers
          in
          fiber_stack := List.tl !fiber_stack;
          fiber_idx := parent_fiber_idx;
          fiber := parent_fiber;
          (* Increment the child's dirty bit, so that it can't be resumed
             multiple times. *)
          let child_dirty = !next_dirty in
          incr next_dirty;
          let {
            dirty = child_dirty_cell;
            pc = child_pc;
            ret_size = child_ret_size;
            _;
          } =
            List.assoc child_idx !all_fibers
          in
          child_dirty_cell := child_dirty;
          child_pc := i + 1;
          (* We now need to put the fiber that just finished onto the parent
             fiber's stack. The stack layout is

             fibdirty
             fibidx
             zeroed_return
             bit           < stack top
          *)
          Fiber.push_int !fiber child_dirty;
          Fiber.push_int !fiber child_idx;
          Fiber.push_zeroed !fiber child_ret_size;
          (* pending = 0 *)
          Fiber.push_int !fiber 0;
          (* Continue on the parent fiber *)
          go !parent_pc
    | Spawn { proc; args_size; ret_size } ->
        (* Grab the arguments and pc for the fiber *)
        let child_pc = List.assoc proc label_tbl in
        let arg = Fiber.pop_block !fiber args_size in
        (* Setup and associate the new child fiber *)
        let child_fiber = Fiber.make ~ret:ret_size ~arg in
        let child_idx = !next_idx in
        incr next_idx;
        let child_cell =
          { fiber = child_fiber; dirty = ref (-1); pc = ref (-1); ret_size }
        in
        all_fibers := (child_idx, child_cell) :: !all_fibers;
        (* Store the parent's program counter, and yield control to the child. *)
        let parent_idx = !fiber_idx in
        (List.assoc parent_idx !all_fibers).pc := i + 1;
        fiber_stack := parent_idx :: !fiber_stack;
        fiber_idx := child_idx;
        fiber := child_fiber;
        go child_pc
    | Resume return_size ->
        (*
           fibdirty
           fibidx
           zeroed_return
           bit           < stack top
        *)
        let bit = Fiber.pop_int !fiber in
        if bit = 1 then (
          (* The fiber is already done; nothing to resume. Simply leave it on
             the stack. *)
          Fiber.push_int !fiber bit;
          go (i + 1))
        else
          (* Pop off the zeroed return value, since we don't care about that. *)
          let _ = Fiber.pop_block !fiber return_size in
          let child_idx = Fiber.pop_int !fiber in
          let expected_dirty = Fiber.pop_int !fiber in
          let { fiber = child_fiber; dirty = child_dirty; pc = child_pc; _ } =
            List.assoc child_idx !all_fibers
          in
          if !child_dirty <> expected_dirty then
            failwith
              "Cannot resume a fiber multiple times! Be sure to resume only \
               the most recently invoked version of a fiber.";
          (* Store the parent's program counter, then yield control to the
             resumed child. *)
          let parent_idx = !fiber_idx in
          (List.assoc parent_idx !all_fibers).pc := i + 1;
          fiber_stack := parent_idx :: !fiber_stack;
          fiber_idx := child_idx;
          fiber := child_fiber;
          go !child_pc
    | Push locator ->
        Fiber.push !fiber locator;
        go (i + 1)
    | Store fp_offset ->
        Fiber.store !fiber fp_offset;
        go (i + 1)
    | SpAdd n ->
        Fiber.sp_add !fiber n;
        go (i + 1)
    | SpSub n ->
        Fiber.sp_sub !fiber n;
        go (i + 1)
    | SpRestoreFp ->
        Fiber.reset_to_fp !fiber;
        go (i + 1)
    | Jmp l ->
        let j = List.assoc l label_tbl in
        go j
    | Jmpz l ->
        let isz = Fiber.pop_int !fiber = 0 in
        let j = if isz then List.assoc l label_tbl else i + 1 in
        go j
    | Jmprel1 ->
        let rel = Fiber.pop_int !fiber in
        go (i + 1 + rel)
    | Call proc ->
        Fiber.setup_new_frame !fiber ~pc:(i + 1);
        let j = List.assoc proc label_tbl in
        go j
    | Ret -> (
        (* Restore to frame pointer, then restore the old frame. *)
        Fiber.reset_to_fp !fiber;
        match Fiber.restore_old_frame !fiber with
        | `Pc j -> go j
        | `Done ret_val ->
            if !fiber_idx = 0 then
              (* Main fiber returning is the end of the program. *)
              ret_val
            else
              (* Child fiber has completed. We need to return to the parent
                 and set up the completed return value. *)
              let parent_fiber_idx = List.hd !fiber_stack in
              let { fiber = parent_fiber; pc = parent_pc; _ } =
                List.assoc parent_fiber_idx !all_fibers
              in
              fiber_stack := List.tl !fiber_stack;
              fiber_idx := parent_fiber_idx;
              fiber := parent_fiber;
              (*
                 fibdirty
                 fibidx
                 zeroed_return
                 bit           < stack top

                 fibdirty, fibidx are irrelevant since the fiber has completed.
              *)
              Fiber.push_int !fiber (-1);
              Fiber.push_int !fiber (-1);
              Fiber.push_block !fiber ret_val;
              (* done = 1 *)
              Fiber.push_int !fiber 1;
              go !parent_pc)
  in
  go i_main

let interp { procs; ret_size; ret_ty } =
  let bbs = bbs_of_procs procs in
  let instrs, label_tbl = build_instruction_table bbs in
  let main_fiber = Fiber.make ~ret:ret_size ~arg:Fiber.empty_block in
  let ret_val = eval instrs label_tbl main_fiber ret_size in
  let ret_words = Fiber.vals_of_block ret_val in
  (ret_words, ret_ty)
