(** Bytecode for the compiler VM. *)

open Vm_debug

type label = [ `Label of string ] [@@deriving show]
type locator = [ `Imm of int | `FpOffset of int ]

let main = `Label "@main"

type op =
  | Eq
  | Lt
  | Sub
  | Add
  | Mul
  | Yield
  | Spawn of { proc : label; args_size : int; ret_size : int }
      (**
     call_args
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
  | Jmprel1 (* jump relative to 1 + the integer on the top of the stack. *)
  | Call of label
  | Ret

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
  | Spawn { proc = `Label proc; args_size; ret_size } ->
      fprintf f "spawn %s %d %d" proc args_size ret_size
  | Resume n -> fprintf f "resume %d" n
  | Push l ->
      fprintf f "push ";
      pp_locator f l
  | Store n -> fprintf f "store-into fp[%d]" n
  | SpAdd n -> fprintf f "sp-add %d" n
  | SpSub n -> fprintf f "sp-sub %d" n
  | Jmp (`Label l) -> fprintf f "jmp %s" l
  | Jmpz (`Label l) -> fprintf f "jmpz %s" l
  | Jmprel1 -> fprintf f "jmprel1"
  | Call (`Label l) -> fprintf f "call %s" l
  | Ret -> fprintf f "ret");
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

let pp_proc symbols f { name = `Label proc; blocks = bbs; debug_frame } =
  let open Format in
  fprintf f "@[<v 0>%s: {@," proc;
  pp_debug_frame symbols f debug_frame;
  fprintf f "@,";
  List.iter
    (fun op ->
      pp_bb f op;
      fprintf f "@,")
    bbs;
  fprintf f "}@]"

let pp_program symbols f procs =
  let open Format in
  fprintf f "@[<v 0>";
  Util.intersperse f ""
    (fun f not_first proc ->
      if not_first then fprintf f "@,@,";
      pp_proc symbols f proc)
    procs;
  fprintf f "@]"

let string_of_op op = Util.with_buffer (fun f -> pp_op f op) Util.default_width

let string_of_program ?(width = Util.default_width) symbols (program : program)
    =
  Util.with_buffer (fun f -> pp_program symbols f program.procs) width
