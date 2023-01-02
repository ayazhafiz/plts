open Ast

(* name *)

let preprocess tys =
  let replace tbl k v =
    let tbl' = List.remove_assoc k !tbl in
    tbl := (k, v) :: tbl'
  in
  let _update tbl k f d =
    let v = match List.assoc_opt k !tbl with None -> d | Some v -> f v in
    replace tbl k v
  in
  let claimed = ref [] in
  let hits = ref [] in
  let rec go_ty t =
    match !t with
    | Unbd _ -> ()
    | Link t -> go_ty t
    | Content TBool -> ()
    | Content TInt -> ()
    | Content (TFnFx (in', out', `Stk stack_shape)) ->
        go_ty in';
        go_ty out';
        List.iter go_ty stack_shape
  in
  List.iter go_ty tys;
  (List.rev !claimed, List.rev !hits)

let fresh_name_generator () =
  let tbl = ref [] in
  fun hint ->
    let rec find_named n i =
      let cand = match i with 0 -> n | i -> Printf.sprintf "%s%d" n i in
      if List.mem cand !tbl then find_named n (i + 1) else cand
    in
    let rec find_unnamed n =
      let letter = Char.chr @@ (97 + (n mod 26)) in
      let extra = max 0 (n - 25) in
      let cand =
        if extra = 0 then Char.escaped letter
        else Printf.sprintf "%c%d" letter extra
      in
      if List.mem cand !tbl then find_unnamed (n + 1) else cand
    in
    let name =
      match hint with Some hint -> find_named hint 0 | None -> find_unnamed 0
    in
    tbl := name :: !tbl;
    name

let name_vars tys =
  let claimed, hits = preprocess tys in
  let fresh_name = fresh_name_generator () in
  let names' =
    List.map (fun (i, name) -> (i, fresh_name (Some name))) claimed
  in
  let names'' =
    List.map
      (fun (i, hits) ->
        let name = if hits == 1 then "*" else fresh_name None in
        (i, name))
      hits
  in
  names' @ names''

type named_vars = (int * string) list

let pp_ty (_names : named_vars) f t =
  let open Format in
  let rec go t =
    match !t with
    | Unbd i -> fprintf f "<?%d>" i
    | Link t -> go t
    | Content TBool -> fprintf f "bool"
    | Content TInt -> fprintf f "int"
    | Content (TFnFx (in', out, `Stk stack_shape)) ->
        fprintf f "@[<hov 2>";
        go in';
        fprintf f "@ -> ";
        go out;
        if List.length stack_shape > 0 then (
          fprintf f "@[<hov 2>@ ^{";
          Util.intersperse f ", " (fun _ _ t -> go t) stack_shape;
          fprintf f "}@]");
        fprintf f "@]"
  in
  fprintf f "@[<v 0>";
  go t;
  fprintf f "@]"

let string_of_ty width ty = Util.with_buffer (fun f -> pp_ty [] f ty) width
