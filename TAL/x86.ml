open Util

(** Word size for targeted x86-64 *)
let word_size = 8

type x86_register =
  | Rax  (** return value register *)
  | Rbx
  | Rcx
  | Rdx
  | Rbp  (** base pointer - okay to use since our programs are in CPS *)
  | Rsp
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

let string_of_x86_register = function
  | Rax -> "rax"
  | Rbx -> "rbx"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rbp -> "rbp"
  | Rsp -> "rsp"
  | Rsi -> "rsi"
  | Rdi -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let all_registers =
  [
    Rax; Rbx; Rcx; Rdx; Rbp; Rsp; Rsi; Rdi; R8; R9; R10; R11; R12; R13; R14; R15;
  ]

let available_registers =
  List.filter
    (function
      | Rbp | Rsp ->
          (* Reserved for stack management *)
          false
      | _ -> true)
    all_registers

type regmap = {
  fresh : unit -> register;
  rax : register;
  rsp : register;
  rbp : register;
}

module RegSet = Set.Make (struct
  type t = x86_register

  let compare = compare
end)

type 'r instr =
  | Add of 'r * 'r
  | AddImm of 'r * int
  | Sub of 'r * 'r
  | SubImm of 'r * int
  | Mul of 'r * 'r
  | MulImm of 'r * int
  | Mov of 'r * 'r
  | MovImm of 'r * int
  | MovLab of 'r * label
  | MovLd of 'r * 'r * int  (** mov rd, [rs + off] *)
  | MovSt of 'r * int * 'r  (** mov [rd + off], rs *)
  | Jmp of label
  | Jmpr of 'r
  | Jnz of label
  | Jnzr of 'r
  | Cmp of 'r * int
  | Opaque of string

type 'r block = label * 'r instr list

type 'r program = 'r block list * regmap

type reified_program = x86_register block list

(*** Pretty printing ***)

let pp_r rmap f r =
  let open Format in
  let reg =
    if r = rmap.rax then "rax"
    else if r = rmap.rsp then "rsp"
    else if r = rmap.rbp then "rbp"
    else
      let (R r) = r in
      "r" ^ string_of_int r
  in
  pp_print_string f reg

let pp_instr fmt_reg f =
  let open Format in
  let pp_r r = pp_print_string f (fmt_reg r) in
  let pp_displacement d =
    if d = 0 then ()
    else if d < 0 then fprintf f " - %d" (-d)
    else fprintf f " + %d" d
  in
  function
  | Add (rd, rs) ->
      fprintf f "add ";
      pp_r rd;
      fprintf f ", ";
      pp_r rs
  | AddImm (rd, i) ->
      fprintf f "add ";
      pp_r rd;
      fprintf f ", %d" i
  | Sub (rd, rs) ->
      fprintf f "sub ";
      pp_r rd;
      fprintf f ", ";
      pp_r rs
  | SubImm (rd, i) ->
      fprintf f "sub ";
      pp_r rd;
      fprintf f ", %d" i
  | Mul (rd, rs) ->
      fprintf f "mul ";
      pp_r rd;
      fprintf f ", ";
      pp_r rs
  | MulImm (rd, i) ->
      fprintf f "mul ";
      pp_r rd;
      fprintf f ", %d" i
  | Mov (rd, rs) ->
      fprintf f "mov ";
      pp_r rd;
      fprintf f ", ";
      pp_r rs
  | MovImm (rd, i) ->
      fprintf f "mov ";
      pp_r rd;
      fprintf f ", %d" i
  | MovLab (rd, l) ->
      fprintf f "mov ";
      pp_r rd;
      fprintf f ", ";
      pp_l f l
  | MovLd (rd, rs, i) ->
      fprintf f "mov ";
      pp_r rd;
      fprintf f ", [";
      pp_r rs;
      pp_displacement i;
      fprintf f "]"
  | MovSt (rd, i, rs) ->
      fprintf f "mov qword [";
      pp_r rd;
      pp_displacement i;
      fprintf f "], ";
      pp_r rs
  | Jmp l ->
      fprintf f "jmp ";
      pp_l f l
  | Jmpr r ->
      fprintf f "jmp ";
      pp_r r
  | Jnz l ->
      fprintf f "jnz ";
      pp_l f l
  | Jnzr r ->
      fprintf f "jnz ";
      pp_r r
  | Cmp (r, i) ->
      fprintf f "cmp ";
      pp_r r;
      fprintf f ", %d" i
  | Opaque asm -> pp_print_string f asm

let pp_blocks fmt_reg f blocks =
  let open Format in
  fprintf f "@[<v>";
  let lasti = List.length blocks - 1 in
  List.iteri
    (fun i (Lab l, is) ->
      fprintf f "@[<v 2>%s:@,@[<v>" l;
      let lasti' = List.length is - 1 in
      List.iteri
        (fun i instr ->
          pp_instr fmt_reg f instr;
          if i <> lasti' then fprintf f "@,")
        is;
      fprintf f "@]@]";
      if i <> lasti then fprintf f "@,")
    blocks;
  fprintf f "@]"

let string_of_instr i =
  with_buffer (fun f -> pp_instr string_of_x86_register f i) 80

let string_of_prog fmt_reg blocks =
  let prelude =
    String.trim
      {|
; syntax:nasm
; nasm -f64, ld -L <libc> -no_pie -e _entry
BITS 64
section .text
extern _exit
global _entry
|}
  in
  let body = with_buffer (fun f -> pp_blocks fmt_reg f blocks) 80 in
  prelude ^ "\n\n" ^ body

(*** Register allocation ***)

module Live = struct
  type numbered_block = label * (int * register instr) list

  type numbered_program = numbered_block list * regmap

  let number_blocks blocks =
    let i = ref 0 in
    List.map
      (fun (l, bl) ->
        ( l,
          List.map
            (fun instr ->
              incr i;
              (!i, instr))
            bl ))
      blocks

  let unnumber_blocks =
    List.map (fun (l, bl) -> (l, List.map (fun (_, i) -> i) bl))

  let number_program (p : register program) : numbered_program =
    let blocks, rm = p in
    let nblocks = number_blocks blocks in
    (nblocks, rm)

  let unnumber_program ((blocks, rm) : numbered_program) : register program =
    let blocks' = unnumber_blocks blocks in
    (blocks', rm)

  let uses_of_instr =
    let open IntSet in
    function
    | Add (R r1, R r2)
    | Sub (R r1, R r2)
    | Mul (R r1, R r2)
    | MovSt (R r1, _, R r2) ->
        of_list [ r1; r2 ]
    | AddImm (R r, _)
    | SubImm (R r, _)
    | MulImm (R r, _)
    | Mov (_, R r)
    | MovLd (_, R r, _)
    | Jmpr (R r)
    | Jnzr (R r)
    | Cmp (R r, _) ->
        singleton r
    | MovImm _ | MovLab _ | Jmp _ | Jnz _ | Opaque _ -> empty

  let defs_of_instr =
    let open IntSet in
    function
    | Add (R r, _)
    | Sub (R r, _)
    | Mul (R r, _)
    | MovImm (R r, _)
    | MovLab (R r, _)
    | AddImm (R r, _)
    | SubImm (R r, _)
    | MulImm (R r, _)
    | Mov (R r, _)
    | MovLd (R r, _, _) ->
        singleton r
    | Jmpr _ | Jnzr _ | Cmp _ | Jmp _ | Jnz _ | MovSt _ | Opaque _ -> empty

  type live_data = {
    (* TODO: could be more efficient by using mutable sets *)
    mutable live_in : IntSet.t;
    mutable live_out : IntSet.t;
    instr : register instr;
  }

  type interval = { mutable start : int; mutable fin : int }

  let live_intervals (blocks : numbered_block list) =
    let instr_data =
      List.map snd blocks
      |> List.concat_map (fun block ->
             List.map
               (fun (i, instr) ->
                 (i, { live_in = IntSet.empty; live_out = IntSet.empty; instr }))
               block)
      |> List.to_seq |> Hashtbl.of_seq
    in
    let rec solve () =
      let changed = ref false in
      Hashtbl.iter
        (fun i node ->
          let live_in, live_out = (node.live_in, node.live_out) in
          let defs = defs_of_instr node.instr in
          let uses = uses_of_instr node.instr in
          (* A register is live-in if (1) it is used here or (2) it's live-out
             here, yet not defined here. *)
          let live_in' = IntSet.union uses (IntSet.diff live_out defs) in
          let live_out' =
            match Hashtbl.find_opt instr_data (i + 1) with
            | Some succ ->
                (* A register is live-out if it is live-in at a successor. *)
                IntSet.union live_out succ.live_in
            | None -> live_out
          in
          changed :=
            !changed
            || not IntSet.(equal live_in live_in' && equal live_out live_out');
          node.live_in <- live_in';
          node.live_out <- live_out')
        instr_data;
      if !changed then solve ()
    in
    solve ();
    let all_regs =
      Hashtbl.to_seq_values instr_data
      |> List.of_seq
      |> List.map (fun { instr; _ } ->
             IntSet.union (defs_of_instr instr) (uses_of_instr instr))
      |> IntSet.(List.fold_left union empty)
    in
    let intervals =
      IntSet.to_seq all_regs
      |> Seq.map (fun r ->
             (r, { start = Hashtbl.length instr_data + 1; fin = -1 }))
      |> Hashtbl.of_seq
    in
    Hashtbl.iter
      (fun i { live_in; _ } ->
        IntSet.iter
          (fun r ->
            let d = Hashtbl.find intervals r in
            d.start <- min d.start i;
            d.fin <- max d.fin i)
          live_in)
      instr_data;
    Hashtbl.to_seq intervals |> IntMap.of_seq

  (** Linear Scan Register Allocation, as in Poletto and Sarkar, 1999. *)
  let linear_scan_ra intervals next_stack_loc pre_allocated =
    let intervals =
      List.filter (fun (i, _) -> not (List.mem (R i) pre_allocated)) intervals
      |> List.sort (fun (_, i) (_, j) -> compare i.start j.start)
    in
    let active = ref [] in
    let ralloc = Hashtbl.create (List.length intervals) in
    let spilled = Hashtbl.create (List.length intervals) in
    let free = ref (RegSet.of_list available_registers) in
    let expire_old_intervals i =
      let to_remove = List.filter (fun (_, j) -> j.fin < i.start) !active in
      active := List.filter (fun j -> not (List.mem j to_remove)) !active;
      List.iter
        (fun (r, _) -> free := RegSet.add (Hashtbl.find ralloc r) !free)
        to_remove
    in
    let spill_at_interval (newr, newiv) =
      let spillr, _ =
        List.fold_left
          (fun (spillr, spillfin) (candr, { fin; _ }) ->
            if fin > spillfin then (candr, fin) else (spillr, spillfin))
          (newr, newiv.fin) !active
      in
      let stkloc = next_stack_loc () in
      Hashtbl.add spilled spillr stkloc;
      if spillr <> newr then (
        Hashtbl.add ralloc newr (Hashtbl.find ralloc spillr);
        active := List.filter (fun (r, _) -> r <> spillr) !active;
        active := (newr, newiv) :: !active)
    in
    List.iter
      (fun (i, iv) ->
        expire_old_intervals iv;
        if RegSet.is_empty !free then spill_at_interval (i, iv)
        else
          let r = RegSet.choose !free in
          free := RegSet.remove r !free;
          Hashtbl.add ralloc i r;
          active := (i, iv) :: !active)
      intervals;
    (ralloc, spilled)

  let commit_registers regmap ralloc =
    let rw (R ri as r) =
      if r = regmap.rax then Rax
      else if r = regmap.rsp then Rsp
      else if r = regmap.rbp then Rbp
      else Hashtbl.find ralloc ri
    in
    let rwinstr = function
      | Add (r1, r2) -> Add (rw r1, rw r2)
      | AddImm (r, i) -> AddImm (rw r, i)
      | Sub (r1, r2) -> Sub (rw r1, rw r2)
      | SubImm (r, i) -> SubImm (rw r, i)
      | Mul (r1, r2) -> Mul (rw r1, rw r2)
      | MulImm (r, i) -> MulImm (rw r, i)
      | Mov (r1, r2) -> Mov (rw r1, rw r2)
      | MovImm (r, i) -> MovImm (rw r, i)
      | MovLab (r, l) -> MovLab (rw r, l)
      | MovLd (r1, r2, i) -> MovLd (rw r1, rw r2, i)
      | MovSt (r1, i, r2) -> MovSt (rw r1, i, rw r2)
      | Jmp l -> Jmp l
      | Jmpr r -> Jmpr (rw r)
      | Jnz l -> Jnz l
      | Jnzr r -> Jnzr (rw r)
      | Cmp (r, i) -> Cmp (rw r, i)
      | Opaque asm -> Opaque asm
    in
    List.map (function l, bl ->
        let bl' = List.map rwinstr bl in
        (l, bl'))

  let commit_spills regmap spilled =
    let rbp = regmap.rbp in
    let rwblock (l, instrs) =
      let new_instrs = ref [] in
      let emit i = new_instrs := i :: !new_instrs in
      let use (R r) =
        match Hashtbl.find_opt spilled r with
        | Some rbp_off ->
            let r = regmap.fresh () in
            emit (MovLd (r, rbp, -rbp_off));
            r
        | None -> R r
      in
      let def (R r) =
        match Hashtbl.find_opt spilled r with
        | Some _ -> regmap.fresh ()
        | None -> R r
      in
      let mov_spill (R into) from =
        match Hashtbl.find_opt spilled into with
        | Some rbp_off -> emit (MovSt (rbp, -rbp_off, from))
        | None -> ()
      in
      let rwinstr = function
        | Add (r1, r2) ->
            let r1' = use r1 in
            let r2' = use r2 in
            emit (Add (r1', r2'));
            mov_spill r1 r1'
        | AddImm (r1, i) ->
            let r1' = use r1 in
            emit (AddImm (r1', i));
            mov_spill r1 r1'
        | Sub (r1, r2) ->
            let r1' = use r1 in
            let r2' = use r2 in
            emit (Sub (r1', r2'));
            mov_spill r1 r1'
        | SubImm (r1, i) ->
            let r1' = use r1 in
            emit (SubImm (r1', i));
            mov_spill r1 r1'
        | Mul (r1, r2) ->
            let r1' = use r1 in
            let r2' = use r2 in
            emit (Mul (r1', r2'));
            mov_spill r1 r1'
        | MulImm (r1, i) ->
            let r1' = use r1 in
            emit (MulImm (r1', i));
            mov_spill r1 r1'
        | Mov (rd, rs) ->
            let rs' = use rs in
            let rd' = def rd in
            emit (Mov (rd', rs'));
            mov_spill rd rd'
        | MovImm (r, i) ->
            let r' = def r in
            emit (MovImm (r', i));
            mov_spill r r'
        | MovLab (r, l) ->
            let r' = def r in
            emit (MovLab (r', l));
            mov_spill r r'
        | MovLd (rd, rs, off) ->
            let rs' = use rs in
            let rd' = def rd in
            emit (MovLd (rd', rs', off));
            mov_spill rd rd'
        | MovSt (rd, off, rs) ->
            let rd' = use rd in
            let rs' = use rs in
            emit (MovSt (rd', off, rs'))
        | Jmp l -> emit (Jmp l)
        | Jmpr r ->
            let r' = use r in
            emit (Jmpr r')
        | Jnz l -> emit (Jnz l)
        | Jnzr r ->
            let r' = use r in
            emit (Jnzr r')
        | Cmp (r, i) ->
            let r' = use r in
            emit (Cmp (r', i))
        | Opaque asm -> emit (Opaque asm)
      in
      List.iter rwinstr instrs;
      let instrs' = List.rev !new_instrs in
      (l, instrs')
    in
    List.map rwblock

  let print_live_intervals rmap intervals =
    let open Format in
    with_buffer
      (fun f ->
        fprintf f "@[<v>";
        IntMap.to_seq intervals
        |> Seq.iter (fun (r, { start; fin }) ->
               pp_r rmap f (R r);
               fprintf f ": %d -> %d@," start fin);
        fprintf f "@]")
      80

  let regalloc (blocks, regmap) =
    let next_rbp_offset = ref 0 in
    let next_stack_loc () =
      next_rbp_offset := !next_rbp_offset + word_size;
      !next_rbp_offset
    in
    let rec solve blocks =
      let blocks = number_blocks blocks in
      let reg_intervals =
        live_intervals blocks |> IntMap.to_seq |> List.of_seq
      in
      let ralloc, spilled =
        linear_scan_ra reg_intervals next_stack_loc
          [ regmap.rbp; regmap.rsp; regmap.rax ]
      in
      let blocks' = unnumber_blocks blocks in
      if Hashtbl.length spilled = 0 then
        let reified = commit_registers regmap ralloc blocks' in
        let mov_rbp_rsp = Mov (Rbp, Rsp) in
        let alloc_spilled = SubImm (Rsp, !next_rbp_offset) in
        let reified' =
          List.map
            (function
              | Lab "_entry", is ->
                  (Lab "_entry", mov_rbp_rsp :: alloc_spilled :: is)
              | b -> b)
            reified
        in
        reified'
      else
        let blocks'' = commit_spills regmap spilled blocks' in
        solve blocks''
    in
    solve blocks
end

let optimize blocks =
  let rec optimize_block = function
    | [] -> []
    | Mov (r1, r2) :: rest when r1 = r2 -> optimize_block rest
    | MovSt (r1, off, r2) :: MovLd (r2', r1', off') :: rest
      when r1 = r1' && r2 = r2' && off = off' ->
        (* mov [r1 + off], r2
           mov r2, [r1 + off]
           We cannot eliminate the first move because it is side-effectful, but we
           can eliminate the second as it is redundant. *)
        optimize_block (MovSt (r1, off, r2) :: rest)
    | MovLd (r1, r2, off) :: MovSt (r2', off', r1') :: rest
      when r1 = r1' && r2 = r2' && off = off' ->
        (* mov r1, [r2 + off]
           mov [r2 + off], r1
           We cannot eliminate the first move, but we can eliminate the second. *)
        optimize_block (MovLd (r1, r2, off) :: rest)
    | SubImm (r, i) :: SubImm (r', i') :: rest when r = r' ->
        optimize_block (SubImm (r, i + i') :: rest)
    | i :: rest -> i :: optimize_block rest
  in
  List.map (fun (l, bl) -> (l, optimize_block bl)) blocks

(*** Emulation ***)

type rt_value = L of label | I of int

let string_of_rt_value = function L (Lab l) -> l | I i -> string_of_int i

let reg_find = Hashtbl.find

let int_of regs r =
  match reg_find regs r with
  | I i -> i
  | L _ ->
      failwith (Printf.sprintf "%s is not an int" (string_of_x86_register r))

let label_of regs r =
  match reg_find regs r with L l -> l | I _ -> failwith "not a label"

let emulate blocks =
  let regs = Hashtbl.create 128 in
  let memory = Hashtbl.create 128 in
  Hashtbl.add regs Rsp (I 0);
  let zero_flag = ref false in
  let rec go = function
    | [] -> ()
    | Add (r1, r2) :: rest ->
        Hashtbl.add regs r1 (I (int_of regs r1 + int_of regs r2));
        go rest
    | AddImm (r, i) :: rest ->
        Hashtbl.add regs r (I (int_of regs r + i));
        go rest
    | Sub (r1, r2) :: rest ->
        Hashtbl.add regs r1 (I (int_of regs r1 - int_of regs r2));
        go rest
    | SubImm (r, i) :: rest ->
        Hashtbl.add regs r (I (int_of regs r - i));
        go rest
    | Mul (r1, r2) :: rest ->
        Hashtbl.add regs r1 (I (int_of regs r1 * int_of regs r2));
        go rest
    | MulImm (r, i) :: rest ->
        Hashtbl.add regs r (I (int_of regs r * i));
        go rest
    | Mov (r1, r2) :: rest ->
        Hashtbl.add regs r1 (reg_find regs r2);
        go rest
    | MovImm (r, i) :: rest ->
        Hashtbl.add regs r (I i);
        go rest
    | MovLab (r, l) :: rest ->
        Hashtbl.add regs r (L l);
        go rest
    | (MovLd (r1, r2, i) as ld) :: rest ->
        let addr = int_of regs r2 + i in
        let v =
          match Hashtbl.find_opt memory addr with
          | Some v -> v
          | None ->
              failwith
                (Printf.sprintf "%s cannot be loaded" (string_of_instr ld))
        in
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
    | Opaque _ :: rest -> go rest
  in
  go (List.assoc (Lab "_entry") blocks);
  reg_find regs Rax

module X86 (I : Tal.Int) = struct
  module Tal = Tal.TAL (I)

  (*** Translation ***)

  let termination_seq =
    [
      (* align rsp according to x86 calling convention *)
      Opaque "and rsp, 0xFFFFFFFFFFFFFFF0";
      Opaque "mov rdi, rax";
      Opaque "call _exit";
    ]

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
          | `Int i -> emit (AddImm (rd, i))
          | `Lab _ -> failwith "cannot add label");
          go is
      | Tal.Seq (Tal.Sub (rd, rs, v), is) ->
          emit (Mov (rd, rs));
          (match trans_sval v with
          | `Reg rs' -> emit (Sub (rd, rs'))
          | `Int i -> emit (SubImm (rd, i))
          | `Lab _ -> failwith "cannot subtract label");
          go is
      | Tal.Seq (Tal.Mul (rd, rs, v), is) ->
          emit (Mov (rd, rs));
          (match trans_sval v with
          | `Reg rs' -> emit (Mul (rd, rs'))
          | `Int i -> emit (MulImm (rd, i))
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
          | `Int i -> emit (MovImm (rd, i))
          | `Lab l -> emit (MovLab (rd, l)));
          go is
      (* Tuple operations are pretty simple.
         - We have already checked (during TAL and previous passes) that
           tuples are allocated before they are stored into or loaded from.
         - The TAL program is entirely in CPS style with no need for [call]
           instructions.
           This means we can allocate tuples directly on the stack as long as we
           respect a load/store convention, namely the usual one. Since our program
           is in CPS, we do not need to manage [rsp] around calls either.

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
          let size = List.length ts * word_size in
          emit (SubImm (regmap.rsp, size));
          emit (Mov (rd, regmap.rsp));
          go is
      | Tal.Seq (Tal.Ld (rd, rs, i), is) ->
          let off = i * word_size in
          emit (MovLd (rd, rs, off));
          go is
      | Tal.Seq (Tal.St (rd, i, rs), is) ->
          let off = i * word_size in
          emit (MovSt (rd, off, rs));
          go is
      | Tal.Jmp v -> (
          match trans_sval v with
          | `Lab j -> emit (Jmp j)
          | `Reg r -> emit (Jmpr r)
          | `Int _ -> failwith "cannot jump to int")
      | Tal.Halt _ ->
          emit (Mov (regmap.rax, R 1));
          List.iter emit termination_seq
    in
    go is;
    List.rev !instrs

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
    let entry = (Lab "_entry", is) in
    let blocks = blocks @ [ entry ] in
    let all_used_regs =
      List.map snd blocks |> List.map used_regs
      |> List.fold_left IntSet.union IntSet.empty
    in
    let regmap =
      let i = ref (IntSet.fold max all_used_regs 0) in
      let fresh () =
        incr i;
        R !i
      in
      let rax = fresh () in
      let rsp = fresh () in
      let rbp = fresh () in
      { fresh; rax; rsp; rbp }
    in
    let blocks = List.map (fun (l, is) -> (l, trans_instr regmap is)) blocks in
    let reified_blocks = Live.regalloc (blocks, regmap) in
    optimize reified_blocks
end
