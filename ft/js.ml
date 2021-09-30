open Ft
open Js_of_ocaml

type a = int

let and_then map = function Ok x -> map x | s -> s

let parse program =
  try Ok (parse_term program) with _ -> Error "Parsing error"

let infer program = parse program |> and_then infer_types

let check program do_infer =
  (if do_infer then infer program else parse program)
  |> and_then (fun tm -> typecheck tm |> Result.map (fun _ -> tm))

let ok s =
  object%js
    val result = Js.(some @@ string s)

    val error = Js.null
  end

let err s =
  object%js
    val result = Js.null

    val error = Js.(some @@ string s)
  end

let ret = function Ok s -> ok s | Error s -> err s

let wrap doit =
  Printexc.record_backtrace true;
  try doit ()
  with e ->
    Error
      (Printexc.record_backtrace false;
       "Internal error. Please report this.\n\n" ^ Printexc.to_string e ^ "\n"
       ^ Printexc.get_backtrace ())

let _ =
  Js.export "ftCheck" (fun ~program ~do_infer ->
      wrap (fun () ->
          check (Js.to_string program) (Js.to_bool do_infer)
          |> Result.map string_of_term)
      |> ret)

let _ =
  Js.export "ftInfer" (fun ~program ->
      wrap (fun () -> infer (Js.to_string program) |> Result.map string_of_term)
      |> ret)

let _ =
  Js.export "subtypeCheck" (fun ~query ->
      match Str.split (Str.regexp_string "<:") (Js.to_string query) with
      | [ s; t ] ->
          let st = wrap (fun () -> Ok (parse_ty s, parse_ty t)) in
          Result.map (fun (s, t) -> string_of_bool (s <: t)) st |> ret
      | _ -> err "Query should be of the form `S <: T` where S and T are types.")
