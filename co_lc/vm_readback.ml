(** Read back evaluated values into the source language *)

type value = Vm_fiber.value

let show_value = Vm_fiber.show_value

exception Bad_value of string

let expect_int l = match List.hd l with `Int n -> (n, List.tl l)
let noloc = Ast.noloc
let noty = ref (Ast.Unbd (-1))

let drop_n vals n =
  let rec dropper = function
    | m, l when n = m -> l
    | m, _ :: l -> dropper (m + 1, l)
    | _, [] -> raise @@ Bad_value "values over before drop end"
  in
  dropper (0, vals)

let rec build_ast symbols vals ty =
  let open Ast in
  let expr, rest =
    match !(unlink ty) with
    | Unbd _ | Link _ -> failwith "unreachable"
    | Content c -> (
        match c with
        | TInt ->
            let int, rest = expect_int vals in
            (Lit (`Int int), rest)
        | TBool ->
            let int, rest = expect_int vals in
            (Lit (`Bool (int = 1)), rest)
        | TTup ts ->
            let rev_ts = List.rev ts in
            let items, rest =
              List.fold_left
                (fun (items, vals) t ->
                  let item, rest_vals = build_ast symbols vals t in
                  (item :: items, rest_vals))
                ([], vals) rev_ts
            in
            (Tup items, rest)
        | TFn (_, lambda_set, _) ->
            let stksize = Vm_layout.stack_size ty in
            let lambda, captures =
              if List.length lambda_set = 1 then List.hd lambda_set
              else
                let (`Int bit) = List.nth vals (stksize - 1) in
                List.nth lambda_set bit
            in

            let captures = List.rev captures in
            let captures, _ =
              List.fold_left
                (fun (items, vals) (x, t) ->
                  let item, rest_vals = build_ast symbols vals t in
                  ((noloc, noty, Var x) :: item :: items, rest_vals))
                ([], vals) captures
            in

            let rest = drop_n vals stksize in
            (App ((noloc, noty, Var lambda), (noloc, noty, Tup captures)), rest)
        | TFiber t ->
            let size = Vm_layout.stack_size t + 3 in
            let t_s = string_of_ty symbols t in

            let items =
              if List.nth vals (size - 1) = `Int 1 then
                let completed, _ = build_ast symbols (drop_n vals 2) t in
                App ((noloc, noty, Var (`Sym "`Done")), completed)
              else Var (`Sym "`Pending")
            in

            let fib = (noloc, noty, Var (`Sym ("Fiber " ^ t_s))) in

            let rest = drop_n vals size in
            (App (fib, (noloc, noty, items)), rest)
        | TTupSparse _ -> failwith "unreachable")
  in
  let node = (noloc, noty, expr) in
  (node, rest)

let readback ?(width = Util.default_width) (symbols : Symbol.t)
    (vals : value list) (ty : Ast.ty) =
  let ast, vals = build_ast symbols vals ty in
  assert (vals = []);
  Ast.string_of_program ~width (Symbol.make ()) ast
