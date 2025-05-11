let default_width = 50

let with_buffer cb width =
  let open Format in
  let b = Buffer.create 32 in
  let f = formatter_of_buffer b in
  pp_set_margin f width;
  cb f;
  pp_print_flush f ();
  Buffer.to_seq b |> String.of_seq

let with_parens f needs_parens inside =
  let open Format in
  if needs_parens then pp_print_string f "(";
  inside ();
  if needs_parens then pp_print_string f ")"

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

let index_of pred l =
  let rec help n = function
    | [] -> raise Not_found
    | c :: rest -> if pred c then n else help (n + 1) rest
  in
  help 0 l

let intersperse f between fn iter =
  List.iteri
    (fun i elt ->
      if i <> 0 then Format.fprintf f "%s" between;
      fn f (i <> 0) elt)
    iter

(* loc printing *)
let string_of_lineco (l, c) = string_of_int l ^ ":" ^ string_of_int c
let string_of_loc (l1, l2) = string_of_lineco l1 ^ "-" ^ string_of_lineco l2

(* region comparisons *)
let deeper (l1, c1) (l2, c2) = l1 > l2 || (l1 = l2 && c1 >= c2)
let shallower lc1 lc2 = deeper lc2 lc1
let within (lc11, lc12) (lc21, lc22) = deeper lc11 lc21 && shallower lc12 lc22

let fresh_name_generator () =
  let taken = ref [] in
  let rec find hint i =
    let cand = if i = 0 then hint else hint ^ string_of_int i in
    if List.mem cand !taken then find hint (i + 1)
    else (
      taken := cand :: !taken;
      cand)
  in
  fun hint -> find hint 0
