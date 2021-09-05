open Language
open CamomileLibrary

let with_buffer cb width =
  let open Format in
  let b = Buffer.create 32 in
  let f = formatter_of_buffer b in
  pp_set_margin f width;
  cb f;
  pp_print_flush f ();
  Buffer.to_seq b |> String.of_seq

let maybe_prettify s pretty =
  match (s, pretty) with
  | "*", true -> "\u{22A4}" (* ⊤ *)
  | "!", true -> "\u{22A5}" (* ⊥ *)
  | "&", true -> "\u{2227}" (* ∧ *)
  | "|", true -> "\u{2228}" (* ∨ *)
  | "->", true -> "\u{2192}" (* → *)
  | "<:", true -> "\u{227A}" (* ≺ *)
  | ":>", true -> "\u{227B}" (* ≻ *)
  | "<|", true -> "\u{25C1}" (* ◁ *)
  | "|>", true -> "\u{25B7}" (* ▷ *)
  | "~=", true -> "\u{2245}" (* ≅ *)
  | s, _ -> s

let pp_ty f pretty t =
  let open Format in
  let rec go parent ty =
    let wrap = should_wrap_ty (ty, parent) in
    match ty with
    | TPrim s -> pp_print_string f s
    | TTop -> pp_print_string f (maybe_prettify "*" pretty)
    | TBot -> pp_print_string f (maybe_prettify "!" pretty)
    | TArrow (t1, t2) ->
        let arrow = maybe_prettify "->" pretty in
        fprintf f "@[<hov 2>";
        if wrap then fprintf f "(";
        go ty t1;
        fprintf f "%s@," arrow;
        go ty t2;
        if wrap then fprintf f ")";
        fprintf f "@]"
    | TOp (op, t1, t2) ->
        let op_s = match op with Sub -> "&" | Sup -> "|" in
        let op_s = maybe_prettify op_s pretty in
        fprintf f "@[<hov 2>";
        if wrap then fprintf f "(";
        go ty t1;
        fprintf f "%s@," op_s;
        go ty t2;
        if wrap then fprintf f ")";
        fprintf f "@]"
  in
  go TBot t

let string_of_ty pretty t = with_buffer (fun f -> pp_ty f pretty t) 80

let string_of_mode_rel pretty mode =
  let s = match mode with Sub -> "<:" | Sup -> ":>" in
  maybe_prettify s pretty

let name_of_split = function
  | SpAnd _ -> "Sp-and"
  | SpArrowR _ -> "Sp-arrowR"
  | SpArrowL _ -> "Sp-arrowL"
  | SpOrL _ -> "Sp-orL"
  | SpOrR _ -> "Sp-orR"

let string_of_split_result =
  let open Printf in
  let ltr = maybe_prettify "<|" true in
  let rtr = maybe_prettify "|>" true in
  let sty = string_of_ty true in
  function
  | SpAnd (a, ab, b)
  | SpArrowR (a, ab, b, _)
  | SpArrowL (a, ab, b, _)
  | SpOrL (a, ab, b, _)
  | SpOrR (a, ab, b, _) ->
      sprintf "%s %s %s %s %s" (sty a) ltr (sty ab) rtr (sty b)

let subsplits_of_split = function
  | SpAnd _ -> []
  | SpArrowR (_, _, _, m)
  | SpArrowL (_, _, _, m)
  | SpOrL (_, _, _, m)
  | SpOrR (_, _, _, m) ->
      [ m ]

let name_of_derivation = function
  | ADPrim _ -> "AD-prim"
  | ADBound _ -> "AD-bound"
  | ADArrow _ -> "AD-arrow"
  | ADDual _ -> "AD-dual"
  | ADAnd _ -> "AD-and"
  | ADAndL _ -> "AD-andL"
  | ADAndR _ -> "AD-andR"

let mode_of_derivation = function
  | ADPrim (m, _, _)
  | ADBound (m, _, _)
  | ADArrow (m, _, _, _)
  | ADDual (m, _, _, _)
  | ADAnd (m, _, _, _, _)
  | ADAndL (m, _, _, _, _)
  | ADAndR (m, _, _, _, _) ->
      m

let string_of_derivation_result pretty =
  let open Printf in
  function
  | ADPrim (m, t1, t2) ->
      sprintf "%s %s %s" (string_of_ty pretty t1)
        (string_of_mode_rel pretty m)
        (string_of_ty pretty t2)
  | ADBound (m, a, t) ->
      sprintf "%s %s %s" (string_of_ty pretty a)
        (string_of_mode_rel pretty m)
        (string_of_ty pretty t)
  | ADArrow (m, a, b, _) ->
      sprintf "%s %s %s" (string_of_ty pretty a)
        (string_of_mode_rel pretty m)
        (string_of_ty pretty b)
  | ADDual (m, a, b, _)
  | ADAnd (m, a, b, _, _)
  | ADAndR (m, a, b, _, _)
  | ADAndL (m, a, b, _, _) ->
      sprintf "%s %s %s" (string_of_ty pretty a)
        (string_of_mode_rel pretty m)
        (string_of_ty pretty b)

let splits_of_derivation = function
  | ADPrim _ | ADBound _ | ADArrow _ | ADDual _ -> []
  | ADAnd (_, _, _, s, _) | ADAndL (_, _, _, s, _) | ADAndR (_, _, _, s, _) ->
      [ s ]

let subderivations_of_derivation = function
  | ADPrim _ | ADBound _ -> []
  | ADArrow (_, _, _, derivs) | ADAnd (_, _, _, _, derivs) -> derivs
  | ADDual (_, _, _, deriv)
  | ADAndL (_, _, _, _, deriv)
  | ADAndR (_, _, _, _, deriv) ->
      [ deriv ]

let vbar = "\u{FF5C}"

let pp_split f =
  let open Format in
  let rec go split =
    let result = string_of_split_result split in
    let dashes = String.make (UTF8.length result) '-' in
    let subsplits = subsplits_of_split split in
    fprintf f "@[<v 2>@[<v 0>%s %s" vbar result;
    fprintf f "@,%s %s %s@]" vbar dashes (name_of_split split);
    List.iter
      (fun subsplit ->
        fprintf f "@,";
        go subsplit)
      subsplits;
    fprintf f "@]"
  in
  go

let pp_derivation_tree f pretty tree =
  let open Format in
  let rec go tree =
    let result = string_of_derivation_result pretty tree in
    let dashes = String.make (UTF8.length result) '-' in
    let splits = splits_of_derivation tree in
    let subderivations = subderivations_of_derivation tree in
    fprintf f "@[<v 2>@[<v 0>%s %s" vbar result;
    fprintf f "@,%s %s %s (%s)@]" vbar dashes (name_of_derivation tree)
      (string_of_mode_rel pretty (mode_of_derivation tree));
    List.iter
      (fun split ->
        fprintf f "@,";
        pp_split f split)
      splits;
    List.iter
      (fun subderiv ->
        fprintf f "@,";
        go subderiv)
      subderivations;
    fprintf f "@]"
  in
  go tree

let pp_judgement f print_debug pretty
    (Judgement (rel, t1, t2, { steps; derivation_trees })) =
  let open Format in
  (let rel_s =
     match rel with
     | `Sub -> "<:"
     | `Sup -> ":>"
     | `Iso -> "~="
     | `Incomp -> "#"
   in
   let rel_s = maybe_prettify rel_s pretty in
   (* debug box *)
   fprintf f "@[<v 2>";
   (* main box *) fprintf f "@[<hov 2>";
   pp_ty f pretty t1;
   fprintf f " %s@ " rel_s;
   pp_ty f pretty t2;
   (* end main box *)
   fprintf f "@]";
   if print_debug then (
     pp_set_max_indent f (Int.max_int / 100);
     fprintf f "@,%s Steps: %d" vbar steps;
     List.iter
       (fun tree ->
         fprintf f "@,";
         pp_derivation_tree f true tree)
       derivation_trees));
  (* end debug box *)
  fprintf f "@]"

let with_buffer cb width =
  let open Format in
  let b = Buffer.create 32 in
  let f = formatter_of_buffer b in
  pp_set_margin f width;
  cb f;
  pp_print_flush f ();
  Buffer.to_seq b |> String.of_seq

let string_of_judgement print_debug pretty j =
  with_buffer (fun f -> pp_judgement f print_debug pretty j) 80

let string_of_query pretty t1 t2 =
  let open Format in
  with_buffer
    (fun f ->
      fprintf f "@[<hov 2>";
      pp_ty f pretty t1;
      fprintf f " ??@ ";
      pp_ty f pretty t2;
      fprintf f "@]")
    80
