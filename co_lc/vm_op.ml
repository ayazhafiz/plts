(** Bytecode for the compiler VM. *)

open Vm_debug

type label = [ `Label of string ]
type locator = [ `Imm of int | `FpOffset of int | label ]

let locator_of_label (`Label s : label) : locator = `Label s

type op =
  | Eq
  | Lt
  | Sub
  | Add
  | Mul
  | Yield
  | Spawn of int
      (**
     call_args
     fn
     ---
     spawn (size_of call_args)
  *)
  | Resume of int  (** size of return value *)
  | Push of locator
  | Drop
  | Store of int  (** store-into fp[offset] *)
  | SpAdd of int
  | Jmp of label
  | Jmpz of label
  | Call of int  (** args byte size *)
  | Ret of int  (** return byte size *)

type basic_block = label * op list

type proc = {
  name : label;
  blocks : basic_block list;
  debug_frame : debug_frame;
}

type program = proc list

let pp_locator f =
  let open Format in
  function
  | `Imm n -> pp_print_int f n
  | `FpOffset n -> fprintf f "fp[%d]" n
  | `Label l -> fprintf f "&%s" l

let pp_op f op =
  let open Format in
  fprintf f "@[";
  (match op with
  | Eq -> pp_print_string f "="
  | Lt -> pp_print_string f "<"
  | Sub -> pp_print_string f "sub"
  | Add -> pp_print_string f "add"
  | Mul -> pp_print_string f "mul"
  | Yield -> pp_print_string f "yield"
  | Spawn n -> fprintf f "spawn %d" n
  | Resume n -> fprintf f "resume %d" n
  | Push l ->
      fprintf f "push ";
      pp_locator f l
  | Drop -> fprintf f "drop"
  | Store n -> fprintf f "store-into fp[%d] " n
  | SpAdd n -> fprintf f "sp-add %d" n
  | Jmp (`Label l) -> fprintf f "jmp %s" l
  | Jmpz (`Label l) -> fprintf f "jmpz %s" l
  | Call n -> fprintf f "call %d" n
  | Ret n -> fprintf f "ret %d" n);
  fprintf f "@]"

let pp_bb f (`Label l, ops) =
  let open Format in
  fprintf f "@[<v 2>%s:@," l;
  Util.intersperse f ""
    (fun f not_first proc ->
      if not_first then fprintf f "@,";
      pp_op f proc)
    ops;
  fprintf f "@]"

let pp_proc f { name = `Label proc; blocks = bbs; debug_frame } =
  let open Format in
  fprintf f "@[<v 0>%s: {@," proc;
  pp_debug_frame f debug_frame;
  fprintf f "@,";
  List.iter
    (fun op ->
      pp_bb f op;
      fprintf f "@,")
    bbs;
  fprintf f "}@]"

let pp_program f procs =
  let open Format in
  fprintf f "@[<v 0>";
  Util.intersperse f ""
    (fun f not_first proc ->
      if not_first then fprintf f "@,@,";
      pp_proc f proc)
    procs;
  fprintf f "@]"

let string_of_program ?(width = Util.default_width) (program : program) =
  Util.with_buffer (fun f -> pp_program f program) width
