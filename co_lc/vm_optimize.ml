(** Simple optimizations for the IR. *)

open Vm_op

type 'a pass = label -> 'a -> bool * 'a

let until_fixpoint : 'a pass list -> label -> 'a -> 'a =
 fun passes proc prog ->
  let rec go prog =
    let rec apply_all changed prog = function
      | [] -> (changed, prog)
      | pass :: others ->
          let changed', prog' = pass proc prog in
          apply_all (changed || changed') prog' others
    in
    let changed, prog' = apply_all false prog passes in
    if not changed then prog' else go prog'
  in
  go prog

(* PASSES OVER BLOCKS *)

let map_over_blocks apply_ops blocks =
  let rec apply_bbs (changed, accu) = function
    | [] -> (changed, List.rev accu)
    | (bb, ops) :: rest ->
        let ops_changed, new_ops = apply_ops (false, []) ops in
        apply_bbs (changed || ops_changed, (bb, new_ops) :: accu) rest
  in
  apply_bbs (false, []) blocks

let map_over_ops apply start ops =
  let rec apply_ops (changed, accu) = function
    | [] -> (changed, List.rev accu)
    | op :: rest -> (
        match apply op with
        | `Nothing -> apply_ops (changed, op :: accu) rest
        | `Drop -> apply_ops (changed, accu) rest
        | `Replace op' -> apply_ops (changed, op' :: accu) rest)
  in
  apply_ops start ops

let collapse_trivial_blocks : blocks pass =
 fun proc blocks ->
  let trivial_blocks =
    List.filter_map
      (fun (bb, ops) ->
        if bb = proc then None
        else
          match ops with
          | [] -> Some (bb, None)
          | [ op ] -> Some (bb, Some op)
          | _ -> None)
      blocks
  in

  let apply = function
    | Jmp l -> (
        match List.assoc_opt l trivial_blocks with
        | Some None -> `Drop
        | Some (Some op) -> `Replace op
        | None -> `Nothing)
    | _ -> `Nothing
  in
  map_over_blocks (map_over_ops apply) blocks

let eliminate_trivial_instructions : blocks pass =
 fun _proc blocks ->
  let rec apply_ops (changed, accu) = function
    | [] -> (changed, List.rev accu)
    | SpAdd 0 :: rest -> apply_ops (true, accu) rest
    | SpSub 0 :: rest -> apply_ops (true, accu) rest
    (* trivial jumps *)
    | Push (`Imm 0) :: Jmpz l :: rest -> apply_ops (true, Jmp l :: accu) rest
    | Push (`Imm n) :: Jmpz _ :: rest when n <> 0 -> apply_ops (true, accu) rest
    (* trivial constant folding *)
    | Push (`Imm n) :: Push (`Imm m) :: Eq :: rest ->
        apply_ops (true, Push (`Imm (if m = n then 1 else 0)) :: accu) rest
    | Push (`Imm n) :: Push (`Imm m) :: Lt :: rest ->
        apply_ops (true, Push (`Imm (if m < n then 1 else 0)) :: accu) rest
    | Push (`Imm n) :: Push (`Imm m) :: Add :: rest ->
        apply_ops (true, Push (`Imm (m + n)) :: accu) rest
    | Push (`Imm n) :: Push (`Imm m) :: Sub :: rest ->
        apply_ops (true, Push (`Imm (m - n)) :: accu) rest
    | Push (`Imm n) :: Push (`Imm m) :: Mul :: rest ->
        apply_ops (true, Push (`Imm (m * n)) :: accu) rest
    (* noop *)
    | op :: rest -> apply_ops (changed, op :: accu) rest
  in
  map_over_blocks apply_ops blocks

let eliminate_unreachable_instructions : blocks pass =
 fun _proc blocks ->
  let rec apply_ops (changed, accu) = function
    | [] -> (changed, List.rev accu)
    (* unconditional jump *)
    | Jmp l :: _ :: _ -> apply_ops (true, Jmp l :: accu) []
    (* noop *)
    | op :: rest -> apply_ops (changed, op :: accu) rest
  in
  map_over_blocks apply_ops blocks

let fold_empty_blocks : blocks pass =
 fun _proc blocks ->
  (* Figure out what blocks to fold into *)
  let _, mappings =
    List.fold_left
      (fun (fold_into, mappings) (bb, ops) ->
        let mappings' =
          match fold_into with
          | Some fold_bb -> (bb, fold_bb) :: mappings
          | None -> mappings
        in
        (* if this block is empty, we can fold it into the last one, or it
           becomes the one to fold into. *)
        let fold_into' =
          if List.length ops = 0 then Some (Option.value fold_into ~default:bb)
          else None
        in
        (fold_into', mappings'))
      (None, []) blocks
  in

  let apply_op = function
    | Jmp l -> (
        match List.assoc_opt l mappings with
        | None -> `Nothing
        | Some l -> `Replace (Jmp l))
    | Jmpz l -> (
        match List.assoc_opt l mappings with
        | None -> `Nothing
        | Some l -> `Replace (Jmpz l))
    | _ -> `Nothing
  in

  let rec apply_bbs (changed, accu) = function
    | [] -> (changed, List.rev accu)
    | (bb, []) :: (_, ops) :: rest -> apply_bbs (true, accu) ((bb, ops) :: rest)
    | [ (_, []) ] -> failwith "unreachable"
    | (bb, ops) :: rest ->
        let ops_changed, new_ops = map_over_ops apply_op (false, []) ops in
        apply_bbs (changed || ops_changed, (bb, new_ops) :: accu) rest
  in
  apply_bbs (false, []) blocks

let optimize_blocks proc blocks =
  let passes =
    [
      eliminate_trivial_instructions;
      eliminate_unreachable_instructions;
      collapse_trivial_blocks;
      fold_empty_blocks;
    ]
  in
  until_fixpoint passes proc blocks

let optimize_procs procs =
  (* no inter-proc optimization for now *)
  List.map
    (fun { name; blocks; debug_frame } ->
      { name; blocks = optimize_blocks name blocks; debug_frame })
    procs

let optimize_program : program -> program =
 fun { procs; ret_size; ret_ty } ->
  { procs = optimize_procs procs; ret_size; ret_ty }
