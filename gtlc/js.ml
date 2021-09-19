open Gtlc
open Js_of_ocaml

let ( >>= ) = Result.bind

let ( >+ ) v f = Result.map f v

let to_elab program = Js.to_string program |> parse >>= elaborate

let to_lifted program opt =
  to_elab program >+ insert_casts >+ lift ~optimize:(Js.to_bool opt)

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

let make_builtin ({ name; ty; doc } : builtin) =
  object%js
    val name = Js.string name

    val ty = Js.string ty

    val doc = Js.string doc
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
       method irCompile program opt =
         wrap (fun () -> to_lifted program opt >+ string_of_lifted_program)
         |> ret

       method tsCompile program opt =
         wrap (fun () -> to_lifted program opt >+ Cgen.typescript) |> ret

       method cCompile program opt =
         wrap (fun () -> to_lifted program opt >+ Cgen.c) |> ret

       method doEval program =
         wrap (fun () -> to_elab program >>= eval >+ string_of_value) |> ret

       method docs =
         List.map make_builtin builtin_docs |> Array.of_list |> Js.array
    end)
