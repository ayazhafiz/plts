module X86 (I : Tal.Int) = struct
  module Tal = Tal.TAL (I)

  type register = Tal.register

  type label = Tal.label

  type instr =
    | Add of register * register
    | Addimm of register * int
    | Sub of register * register
    | Subimm of register * int
    | Mul of register * register
    | Mulimm of register * int
    | Mov of register * register
    | Movimm of register * int
    | Movlab of register * label
    | MovLd of register * register * int  (** mov rd, [rs + off] *)
    | MovSt of register * int * register  (** mov [rd + off], rs *)
    | Jmp of label
    | Jmpr of register
    | Jnz of label
    | Jnzr of register
    | Cmp of register * int

  type block = label * instr list

  type regmap = { fresh : unit -> register; rax : register; rsp : register }

  type program = block list * regmap

  (*** Pretty printing ***)

  let pp_r rmap f r =
    let open Format in
    let reg =
      if r = rmap.rax then "rax"
      else if r = rmap.rsp then "rsp"
      else
        let (R r) = r in
        "r" ^ string_of_int r
    in
    pp_print_string f reg

  let pp_instr rmap f =
    let open Format in
    let pp_l = Tal.pp_l in
    function
    | Add (rd, rs) ->
        fprintf f "add ";
        pp_r rmap f rd;
        fprintf f ", ";
        pp_r rmap f rs
    | Addimm (rd, i) ->
        fprintf f "add ";
        pp_r rmap f rd;
        fprintf f ", %d" i
    | Sub (rd, rs) ->
        fprintf f "sub ";
        pp_r rmap f rd;
        fprintf f ", ";
        pp_r rmap f rs
    | Subimm (rd, i) ->
        fprintf f "sub ";
        pp_r rmap f rd;
        fprintf f ", %d" i
    | Mul (rd, rs) ->
        fprintf f "mul ";
        pp_r rmap f rd;
        fprintf f ", ";
        pp_r rmap f rs
    | Mulimm (rd, i) ->
        fprintf f "mul ";
        pp_r rmap f rd;
        fprintf f ", %d" i
    | Mov (rd, rs) ->
        fprintf f "mov ";
        pp_r rmap f rd;
        fprintf f ", ";
        pp_r rmap f rs
    | Movimm (rd, i) ->
        fprintf f "mov ";
        pp_r rmap f rd;
        fprintf f ", %d" i
    | Movlab (rd, l) ->
        fprintf f "mov ";
        pp_r rmap f rd;
        fprintf f ", ";
        pp_l f l
    | MovLd (rd, rs, i) ->
        fprintf f "mov ";
        pp_r rmap f rd;
        fprintf f ", [";
        pp_r rmap f rs;
        fprintf f " + %d]" i
    | MovSt (rd, i, rs) ->
        fprintf f "mov qword [";
        pp_r rmap f rd;
        fprintf f " + %d], " i;
        pp_r rmap f rs
    | Jmp l ->
        fprintf f "jmp ";
        pp_l f l
    | Jmpr r ->
        fprintf f "jmp ";
        pp_r rmap f r
    | Jnz l ->
        fprintf f "jnz ";
        pp_l f l
    | Jnzr r ->
        fprintf f "jnz ";
        pp_r rmap f r
    | Cmp (r, i) ->
        fprintf f "cmp ";
        pp_r rmap f r;
        fprintf f ", %d" i

  let pp_blocks regmap f blocks =
    let open Format in
    fprintf f "@[<v>";
    let lasti = List.length blocks - 1 in
    List.iteri
      (fun i (Tal.Lab l, is) ->
        fprintf f "@[<v 2>%s:@,@[<v>" l;
        let lasti' = List.length is - 1 in
        List.iteri
          (fun i instr ->
            pp_instr regmap f instr;
            if i <> lasti' then fprintf f "@,")
          is;
        fprintf f "@]@]";
        if i <> lasti then fprintf f "@,")
      blocks;
    fprintf f "@]"

  let string_of_prog (blocks, regmap) =
    Util.with_buffer (fun f -> pp_blocks regmap f blocks) 80

  (*** Translation ***)

  let rec trans_wval = function
    | Tal.WVLabel lab -> `Lab lab
    | Tal.WVInt i -> `Int (I.to_ocaml_int i)
    | Tal.WVJunk _ -> failwith "should not be translating junk words"
    | Tal.WVTyApp (v, _) | WVPack (_, v, _) -> trans_wval v

  let rec trans_sval = function
    | Tal.SVReg r -> `Reg r
    | Tal.SVWord w -> trans_wval w
    | Tal.SVTyApp (v, _) | SVPack (_, v, _) -> trans_sval v

  let trans_instr regmap is =
    let instrs = ref [] in
    let emit i = instrs := i :: !instrs in
    let rec go = function
      | Tal.Seq (Tal.Add (rd, rs, v), is) ->
          emit (Mov (rd, rs));
          (match trans_sval v with
          | `Reg rs' -> emit (Add (rd, rs'))
          | `Int i -> emit (Addimm (rd, i))
          | `Lab _ -> failwith "cannot add label");
          go is
      | Tal.Seq (Tal.Sub (rd, rs, v), is) ->
          emit (Mov (rd, rs));
          (match trans_sval v with
          | `Reg rs' -> emit (Sub (rd, rs'))
          | `Int i -> emit (Subimm (rd, i))
          | `Lab _ -> failwith "cannot subtract label");
          go is
      | Tal.Seq (Tal.Mul (rd, rs, v), is) ->
          emit (Mov (rd, rs));
          (match trans_sval v with
          | `Reg rs' -> emit (Mul (rd, rs'))
          | `Int i -> emit (Mulimm (rd, i))
          | `Lab _ -> failwith "cannot multiply label");
          go is
      | Tal.Seq (Tal.Bnz (rd, j), is) ->
          emit (Cmp (rd, 0));
          (match trans_sval j with
          | `Lab j -> emit (Jnz j)
          | `Reg r -> emit (Jnzr r)
          | `Int _ -> failwith "cannot jump to int");
          go is
      | Tal.Seq ((Tal.Mov (rd, v) | Tal.Unpack (_, rd, v)), is) ->
          (match trans_sval v with
          | `Reg rs -> emit (Mov (rd, rs))
          | `Int i -> emit (Movimm (rd, i))
          | `Lab l -> emit (Movlab (rd, l)));
          go is
      (* Tuple operations are pretty simple.
         - We have already checked (during TAL and previous passes) that
           tuples are allocated before they are stored into or loaded from.
         - The TAL program is entirely in CPS style with no need for [call]
           instructions.
           This means we can allocate tuples directly on the stack as long as we
           respect a load/store convention, namely the usual one. Since our program
           is in CPS, we do not need to manage [rsp] around calls either.
           We target 64-bit machines so all words are 8 bytes.

         Tal.Malloc rcx, ts =>
            sub rsp, (8 * |ts|)
            mov rcx, rsp

            --- old rsp
            .2  |
            .1  |
            .0  |
            --- rsp ~= rcx

         Tal.Ld rdx, rcx, 1 =>
            mov rdx, [rcx + off]  | off = 1 * 8

         Tal.St rcx, 1, rdx =>
            mov [rcx + off], rdx  | off = 1 * 8
      *)
      | Tal.Seq (Tal.Malloc (rd, ts), is) ->
          let size = List.length ts * 8 in
          (* TODO: mapping of fresh temp register to rsp *)
          emit (Subimm (regmap.rsp, size));
          emit (Mov (rd, regmap.rsp));
          go is
      | Tal.Seq (Tal.Ld (rd, rs, i), is) ->
          let off = i * 8 in
          emit (MovLd (rd, rs, off));
          go is
      | Tal.Seq (Tal.St (rd, i, rs), is) ->
          let off = i * 8 in
          emit (MovSt (rd, off, rs));
          go is
      | Tal.Jmp v -> (
          match trans_sval v with
          | `Lab j -> emit (Jmp j)
          | `Reg r -> emit (Jmpr r)
          | `Int _ -> failwith "cannot jump to int")
      | Tal.Halt _ ->
          (* TODO: mapping of fresh temp register to rax *)
          emit (Mov (regmap.rax, R 1))
    in
    go is;
    List.rev !instrs

  module IntSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

  let used_regs =
    let open Tal in
    let open IntSet in
    let rec go_sv = function
      | SVReg (R n) -> singleton n
      | SVWord _ -> empty
      | SVTyApp (v, _) -> go_sv v
      | SVPack (_, v, _) -> go_sv v
    and go_instr = function
      | Add (R r1, R r2, v) | Sub (R r1, R r2, v) | Mul (R r1, R r2, v) ->
          go_sv v |> add r1 |> add r2
      | Bnz (R r, v) -> go_sv v |> add r
      | Ld (R r1, R r2, _) | St (R r1, _, R r2) -> of_list [ r1; r2 ]
      | Malloc (R r, _) -> singleton r
      | Mov (R r, v) -> go_sv v |> add r
      | Unpack (_, R r, v) -> go_sv v |> add r
    and go_instrs = function
      | Seq (i, is) -> union (go_instr i) (go_instrs is)
      | Jmp v -> go_sv v
      | Halt _ -> empty
    in
    go_instrs

  let trans_prog (Tal.P (h, _, is)) =
    let blocks =
      List.map
        (function
          | l, Tal.HVCode (_, _, is) -> (l, is)
          (* Previous passes of the compiler will never have placed a tuple on
             the heap during top-level translation of the program. Tuples only
             appear due to [Tal.Malloc] instructions, and hence are placed on
             the evaluation heap only during [Tal.eval]. Furthermore, only
             code is hoisted to definitions, not tuples. *)
          | _, Tal.HVTup _ -> failwith "unreachable")
        h
    in
    let entry = (Tal.Lab "_entry", is) in
    let blocks = blocks @ [ entry ] in
    let all_used_regs =
      List.map snd blocks |> List.map used_regs
      |> List.fold_left IntSet.union IntSet.empty
    in
    let regmap =
      let i = ref (IntSet.fold max all_used_regs 0) in
      let fresh () =
        incr i;
        Tal.R !i
      in
      let rax = fresh () in
      let rsp = fresh () in
      { fresh; rax; rsp }
    in
    let blocks = List.map (fun (l, is) -> (l, trans_instr regmap is)) blocks in
    (blocks, regmap)

  (*** Emulation ***)

  type rt_value = L of label | I of int

  let string_of_rt_value = function L l -> Tal.sl l | I i -> string_of_int i

  let reg_find = Hashtbl.find

  let int_of regs r =
    match reg_find regs r with I i -> i | L _ -> failwith "not an int"

  let label_of regs r =
    match reg_find regs r with L l -> l | I _ -> failwith "not a label"

  let emulate (blocks, regmap) =
    let regs = Hashtbl.create 128 in
    let memory = Hashtbl.create 128 in
    Hashtbl.add regs regmap.rsp (I 0);
    let zero_flag = ref false in
    let rec go = function
      | [] -> ()
      | Add (r1, r2) :: rest ->
          Hashtbl.add regs r1 (I (int_of regs r1 + int_of regs r2));
          go rest
      | Addimm (r, i) :: rest ->
          Hashtbl.add regs r (I (int_of regs r + i));
          go rest
      | Sub (r1, r2) :: rest ->
          Hashtbl.add regs r1 (I (int_of regs r1 - int_of regs r2));
          go rest
      | Subimm (r, i) :: rest ->
          Hashtbl.add regs r (I (int_of regs r - i));
          go rest
      | Mul (r1, r2) :: rest ->
          Hashtbl.add regs r1 (I (int_of regs r1 * int_of regs r2));
          go rest
      | Mulimm (r, i) :: rest ->
          Hashtbl.add regs r (I (int_of regs r * i));
          go rest
      | Mov (r1, r2) :: rest ->
          Hashtbl.add regs r1 (reg_find regs r2);
          go rest
      | Movimm (r, i) :: rest ->
          Hashtbl.add regs r (I i);
          go rest
      | Movlab (r, l) :: rest ->
          Hashtbl.add regs r (L l);
          go rest
      | MovLd (r1, r2, i) :: rest ->
          let addr = int_of regs r2 + i in
          let v = Hashtbl.find memory addr in
          Hashtbl.add regs r1 v;
          go rest
      | MovSt (r1, i, r2) :: rest ->
          let v = reg_find regs r2 in
          let addr = int_of regs r1 + i in
          Hashtbl.add memory addr v;
          go rest
      | Jmp l :: _ -> go (List.assoc l blocks)
      | Jmpr r :: _ -> go (List.assoc (label_of regs r) blocks)
      | Jnz l :: rest ->
          if not !zero_flag then go (List.assoc l blocks) else go rest
      | Jnzr r :: rest ->
          if not !zero_flag then go (List.assoc (label_of regs r) blocks)
          else go rest
      | Cmp (r, i) :: rest ->
          zero_flag := int_of regs r = i;
          go rest
    in
    go (List.assoc (Tal.Lab "_entry") blocks);
    reg_find regs regmap.rax
end
