(** The VM's implementation of fibers, user-space call stacks also sometimes
    known as green threads.
    This implementation of fibers is used in service of full coroutines in the
    co_lc language; that is, stackful, assymetric coroutines.

    The implementation is largely pedalogical and un-optimized; for example, the
    stack representation is typed, when a production implementation likely would
    not implement it as such. *)

exception Bad_stack of string

let invalid_state s = raise (Bad_stack s)

type word = [ `Int of int | `Label of Vm_op.label ]
type block = word Array.t

let debug_word = `Int 0xAAAAAAAA
let empty_block = Array.make 0 debug_word

module Stack = struct
  type t = { len : int ref; arr : word Array.t ref }

  let make capacity =
    { len = ref 0; arr = ref (Array.make capacity debug_word) }

  let reserve { len; arr } n_more =
    let cap = Array.length !arr in
    if !len + n_more >= cap then
      arr := Array.append !arr (Array.make cap debug_word)

  let get { arr; _ } i = Array.get !arr i
  let set { arr; _ } i word = Array.set !arr i word

  let push vec word =
    reserve vec 1;
    Array.set !(vec.arr) !(vec.len) word;
    incr vec.len

  let pop vec =
    let word = Array.get !(vec.arr) !(vec.len) in
    decr vec.len;
    word

  let extend vec block =
    let block_size = Array.length block in
    reserve vec block_size;
    Array.blit block 0 !(vec.arr) !(vec.len) block_size;
    vec.len := !(vec.len) + block_size

  let splice_off vec block_size =
    let block = Array.make block_size debug_word in
    let arr_start = !(vec.len) - block_size in
    Array.blit !(vec.arr) arr_start block 0 block_size;
    vec.len := arr_start;
    block

  let modify_top { len; arr } f =
    let idx = !len - 1 in
    let word = f @@ Array.get !arr idx in
    Array.set !arr idx word

  let len { len; _ } = !len

  let truncate { len; _ } new_len =
    assert (new_len <= !len);
    len := new_len
end

type t = { stack : Stack.t; fp : int ref }

let make block =
  let stack = Stack.make 64 in
  (*
      Setup initial frame s
      args (block)
      END_pc
      END_fp
      END_sp
      --- current frame
    *)
  Stack.extend stack block;
  Stack.push stack (`Int 0);
  Stack.push stack (`Int 0);
  Stack.push stack (`Int 0);
  let fp = Stack.len stack in
  { stack; fp = ref fp }

let pop { stack; _ } = Stack.pop stack

let pop_int fiber =
  match pop fiber with `Int n -> n | _ -> invalid_state "not an int"

let pop_label fiber =
  match pop fiber with `Label l -> l | _ -> invalid_state "not a label"

let pop_block { stack; _ } block_size = Stack.splice_off stack block_size

let in_place_int { stack; _ } f =
  let modifier = function
    | `Int n -> `Int (f n)
    | _ -> invalid_state "not an int"
  in
  Stack.modify_top stack modifier

let push_int { stack; _ } n = Stack.push stack (`Int n)
let push_label { stack; _ } l = Stack.push stack (`Label l)
let push_block { stack; _ } block = Stack.extend stack block

let push_zeroed fiber n =
  let block = Array.make n (`Int 0) in
  push_block fiber block

let push fiber = function
  | `Imm n -> push_int fiber n
  | `FpOffset n ->
      let word = Stack.get fiber.stack @@ (!(fiber.fp) + n) in
      Stack.push fiber.stack word
  | `Label l -> push_label fiber (`Label l)

let store { stack; fp } fp_offset =
  let idx = !fp + fp_offset in
  let word = Stack.pop stack in
  Stack.set stack idx word

let sp_add fiber n =
  let block = Array.make n debug_word in
  push_block fiber block

let setup_new_frame fiber ~pc =
  (*
    ...args (set elsewhere)
    old_pc
    old_fp
    old_sp
    --- new frame
  *)
  let old_sp = Stack.len fiber.stack in
  push_int fiber pc;
  push_int fiber !(fiber.fp);
  push_int fiber old_sp;
  fiber.fp := Stack.len fiber.stack

let reset_to_fp { stack; fp } = Stack.truncate stack !fp

let restore_old_frame fiber =
  (*
    ...args (set elsewhere)
    --- wanted result
    old_pc
    old_fp
    old_sp
    --- current frame
  *)
  let old_sp = pop_int fiber in
  let old_fp = pop_int fiber in
  let old_pc = pop_int fiber in
  Stack.truncate fiber.stack old_sp;
  fiber.fp := old_fp;
  if old_fp = 0 then `Done else `Pc old_pc
