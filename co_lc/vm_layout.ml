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
      | TFn _ ->
          (* 1 word for the function name *)
          (* TODO: closure data *)
          1
      | TFiber t ->
          (* {bit, return_value, stkidx, stkdirty}
              1    t             1       1
          *)
          1 + stack_size t + 1 + 1)
