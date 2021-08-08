type op = Plus | Minus | Times

let with_buffer cb width =
  let open Format in
  let b = Buffer.create 32 in
  let f = formatter_of_buffer b in
  pp_set_margin f width;
  cb f;
  pp_print_flush f ();
  Buffer.to_seq b |> String.of_seq

module SSet = Set.Make (struct
  type t = string

  let compare = compare
end)

let rec freshen a used = if SSet.mem a used then freshen (a ^ "'") used else a

exception TyErr of string

exception EvalErr of string
