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
          let captures_sizes =
            List.map
              (fun (_, captures) ->
                List.fold_left (fun n (_, t) -> n + stack_size t) 0 captures)
              lambda_set
          in
          let max_captures_size = List.fold_left max 0 captures_sizes in
          (* if the lambda set is unary, no need for a bit to represent where to
              dispatch *)
          let bit_size = if List.length lambda_set > 1 then 1 else 0 in
          bit_size + max_captures_size
      | TFiber t ->
          (* {bit, return_value, stkidx, stkdirty}
              1    t             1       1
          *)
          1 + stack_size t + 1 + 1)
