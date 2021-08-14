type op = Plus | Minus | Times

type label = Lab of string

type register = R of int

let pp_op f =
  let open Format in
  function
  | Plus -> pp_print_string f "+"
  | Minus -> pp_print_string f "-"
  | Times -> pp_print_string f "*"

let do_op op i j =
  match op with Plus -> i + j | Minus -> i - j | Times -> i * j

let pp_r f =
  let open Format in
  function R i -> fprintf f "r%d" i

let pp_l f =
  let open Format in
  function Lab l -> fprintf f "%s" l

let with_buffer cb width =
  let open Format in
  let b = Buffer.create 32 in
  let f = formatter_of_buffer b in
  pp_set_margin f width;
  cb f;
  pp_print_flush f ();
  Buffer.to_seq b |> String.of_seq

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

module SSet = Set.Make (struct
  type t = string

  let compare = compare
end)

let rec freshen a used = if SSet.mem a used then freshen (a ^ "'") used else a

let fresh_generator used =
  let used = ref used in
  fun hint ->
    let rec gen i =
      let cand = if i = 0 then hint else hint ^ string_of_int i in
      if SSet.mem cand !used then gen (i + 1)
      else (
        used := SSet.add cand !used;
        cand)
    in
    gen 0

exception TyErr of string

exception EvalErr of string
