(** Bytecode for the compiler VM. *)

type label = [ `Label of string ]
type locator = [ `Imm of int | `FpOffset of int | label ]

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
  | Push of locator
  | Drop
  | Store of int  (** store-into fp[offset] *)
  | SpAdd of int
  | Jmp of label
  | Jmpz of label
  | Call of int  (** args byte size *)
  | Ret of int  (** return byte size *)

type basic_block = label * op list
type program = basic_block list

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
  | Spawn n -> fprintf f "yield %d" n
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
  List.iter
    (fun op ->
      pp_op f op;
      fprintf f "@,")
    ops;
  fprintf f "@]"

let pp_program f bbs =
  let open Format in
  fprintf f "@[<v 0>";
  Util.intersperse f ""
    (fun f not_first bb ->
      if not_first then fprintf f "@,";
      pp_bb f bb)
    bbs;
  fprintf f "@]"
