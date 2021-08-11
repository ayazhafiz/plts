open Talc
open Js_of_ocaml

let to_a program =
  try
    parse_term program |> F.elaborate |> K.convert |> C.convert |> H.convert
    |> A.convert |> TAL.convert |> Result.ok
  with
  | TyError s | EvalError s -> Error s
  | _ -> Error "Parsing error"

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
  Js.export_all
    (object%js
       method talCompile program =
         wrap (fun () ->
             to_a (Js.to_string program) |> Result.map TAL.string_of_term)
         |> ret

       method talEval program =
         wrap (fun () ->
             to_a (Js.to_string program)
             |> Result.map TAL.eval
             |> Result.map TAL.string_of_value)
         |> ret
    end)
