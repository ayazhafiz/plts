(** Pretty printing of lambda expressions *)

let ident ppf x = Format.fprintf ppf "%s" x

let sequence printer lst ppf =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
    printer ppf lst

(** Split a variable name into base and numerical postfix, e.g.,
    ["x42"] is split into [("x", 42)]. *)
let split s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && '0' <= s.[!i] && s.[!i] <= '9' do
    decr i
  done;
  if !i < 0 || !i = n - 1 then (s, None)
  else
    let k = int_of_string (String.sub s (!i + 1) (n - !i - 1)) in
    (String.sub s 0 (!i + 1), Some k)

(** Given a variable [x] and a list of variable names [xs], find a variant of [x] which
    does not appear in [xs]. *)
let refresh x xs =
  if not (List.mem x xs) then x
  else
    let y, k = split x in
    let k = ref (match k with Some k -> k | None -> 0) in
    while List.mem (y ^ string_of_int !k) xs do
      incr k
    done;
    y ^ string_of_int !k

let rec lambda xs y e ppf =
  let rec collect ({ Zoo.data = e'; _ } as e) =
    match e' with
    | Grammar.Subst (s, e) ->
        let e = Grammar.subst s e in
        collect e
    | Grammar.Lambda (y, e) ->
        let ys, k, e = collect e in
        ((y, k) :: ys, k + 1, e)
    | Grammar.Var _ | Grammar.App _ -> ([], 0, e)
  in
  let ys, k, e = collect e in
  let ys = (y, k) :: ys in
  let ys, _ =
    List.fold_right
      (fun (y, k) (ys, xs) ->
        let y = if Grammar.occurs_freely k e then refresh y xs else "_" in
        (y :: ys, y :: xs))
      ys ([], xs)
  in
  let xs = List.rev ys @ xs in
  Zoo.print_parens ~at_level:3 ppf "λ %t .@ %t" (sequence ident ys) (expr xs e)

(** Prints expression [e] using formatter [ppf]. *)
and expr ?max_level xs e ppf =
  let rec expr ?max_level xs { Zoo.data = e; _ } ppf = expr' ?max_level xs e ppf
  and expr' ?max_level xs e ppf =
    let print ?at_level = Zoo.print_parens ?max_level ?at_level ppf in
    if not (Format.over_max_boxes ()) then
      match e with
      | Grammar.Var k -> print "%s" (List.nth xs k)
      | Grammar.Subst (s, e) ->
          let e = Grammar.subst s e in
          print "%t" (expr ?max_level xs e)
      | Grammar.Lambda (y, e) -> print ~at_level:3 "%t" (lambda xs y e)
      | Grammar.App (e1, e2) ->
          print ~at_level:1 "%t@ %t" (expr ~max_level:1 xs e1)
            (expr ~max_level:0 xs e2)
  in
  expr ?max_level xs e ppf
