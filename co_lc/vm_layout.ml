(** Stack size in integer cells. *)
let rec stack_size t =
  let open Ast in
  match !(unlink t) with
  | Link _ | Unbd _ -> failwith "non-contentful type"
  | Content c -> (
      match c with
      | TInt -> 1
      | TBool -> 1
      | TTup ts -> List.fold_left ( + ) 0 @@ List.map stack_size ts
      | TTupSparse _ -> failwith "non-concrete tuple type"
      | TFn (_, lambda_set, _) ->
          let max_captures_size = lambda_captures_stksize lambda_set in
          (* if the lambda set is unary, no need for a bit to represent where to
              dispatch *)
          let bit_size = if List.length lambda_set > 1 then 1 else 0 in
          bit_size + max_captures_size
      | TFiber t ->
          (* {bit, return_value, fibidx, fibdirty}
              1    t             1       1
          *)
          1 + stack_size t + 1 + 1)

and lambda_captures_stksize lambda_set =
  let captures_sizes =
    List.map (fun (_, captures) -> captures_stksize captures) lambda_set
  in
  List.fold_left max 0 captures_sizes

and captures_stksize captures =
  List.fold_left (fun n (_, t) -> n + stack_size t) 0 captures

type callee_set_storage = {
  tag : int option;
  captures : Ast.captures;
      (** Total stack size of the captures set, needed for padding. *)
  captures_stksize : int;
}
(** How a lambda in a lambda set should represent its data for the set. *)

let callee_set_storage proc lambda_set =
  let captures_stksize = lambda_captures_stksize lambda_set in
  let tag =
    if List.length lambda_set = 1 then None
    else
      let tag =
        fst
        @@ List.find (fun (_, p) -> p = proc)
        @@ List.mapi (fun i (p, _) -> (i, p)) lambda_set
      in
      Some tag
  in
  let captures = List.assoc proc lambda_set in
  { tag; captures; captures_stksize }
