(** Read back evaluated values into the source language *)

type value = Vm_fiber.value

let show_value = Vm_fiber.show_value

exception Bad_value of string

let expect_int l = match List.hd l with `Int n -> (n, List.tl l)

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
        | TFn _ -> failwith "todo readback closures"
        | TFiber t ->
            (* TODO: print pending/done state of fiber *)
            let t_s = string_of_ty symbols t in
            let size = Vm_layout.stack_size t + 3 in
            let rec dropper = function
              | n, l when n = size -> l
              | n, _ :: l -> dropper (n + 1, l)
              | _, [] -> raise @@ Bad_value "values over before fiber"
            in
            let rest = dropper (0, vals) in
            (Var (`Sym ("(Fiber " ^ t_s ^ ")")), rest)
        | TTupSparse _ -> failwith "unreachable")
  in
  let node = (Ast.noloc, ref (Unbd (-1)), expr) in
  (node, rest)

let readback ?(width = Util.default_width) (symbols : Symbol.t)
    (vals : value list) (ty : Ast.ty) =
  let ast, vals = build_ast symbols vals ty in
  assert (vals = []);
  Ast.string_of_program ~width (Symbol.make ()) ast
