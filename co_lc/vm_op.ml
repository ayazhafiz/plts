(** Bytecode for the compiler VM. *)

open Vm_debug

type label = [ `Label of string ] [@@deriving show]
type locator = [ `Imm of int | `FpOffset of int | label ]

let locator_of_label (`Label s : label) : locator = `Label s
let main = `Label "@main"

type op =
  | Eq
  | Lt
  | Sub
  | Add
  | Mul
  | Yield
  | Spawn of { args_size : int; ret_size : int }
      (**
     call_args
     fn
     ---
     spawn
  *)
  | Resume of int  (** size of return value *)
  | Push of locator
  | Store of int  (** store-into fp[offset] *)
  | SpAdd of int
  | SpSub of int
  | Jmp of label
  | Jmpz of label
  | Call
  | Ret of int  (** return byte size *)

type basic_block = label * op list

type proc = {
  name : label;
  blocks : basic_block list;
  debug_frame : debug_frame;
}

type program = { procs : proc list; ret_size : int; ret_ty : Ast.ty }

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
  | Spawn { args_size; ret_size } -> fprintf f "spawn %d %d" args_size ret_size
  | Resume n -> fprintf f "resume %d" n
  | Push l ->
      fprintf f "push ";
      pp_locator f l
  | Store n -> fprintf f "store-into fp[%d] " n
  | SpAdd n -> fprintf f "sp-add %d" n
  | SpSub n -> fprintf f "sp-sub %d" n
  | Jmp (`Label l) -> fprintf f "jmp %s" l
  | Jmpz (`Label l) -> fprintf f "jmpz %s" l
  | Call -> fprintf f "call"
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
  Util.with_buffer (fun f -> pp_program f program.procs) width
