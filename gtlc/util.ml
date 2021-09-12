let with_buffer cb width =
  let open Format in
  let b = Buffer.create 32 in
  let f = formatter_of_buffer b in
  pp_set_margin f width;
  cb f;
  pp_print_flush f ();
  Buffer.to_seq b |> String.of_seq

module S = Set.Make (struct
  type t = string

  let compare = compare
end)

module SMap = Map.Make (struct
  type t = string

  let compare = compare
end)

let freshen x used =
  let rec go n =
    let x' = x ^ string_of_int n in
    if S.mem x' used then go (n + 1) else x'
  in
  go 1

let fresh_generator ?(used = S.empty) () =
  let used = ref used in
  fun hint ->
    let res = freshen hint !used in
    used := S.add res !used;
    res
