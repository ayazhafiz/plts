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
  Js.export "irCompile"
    (fun [@jsdoc {|Compiles to compiler-internal IR|}] ~program ~optimize ->
      wrap (fun () -> to_lifted program optimize >+ string_of_lifted_program)
      |> ret)

let _ =
  Js.export "tsCompile"
    (fun [@jsdoc {|Compiles to TypeScript|}] ~program ~optimize ->
      wrap (fun () -> to_lifted program optimize >+ Cgen.typescript) |> ret)

let _ =
  Js.export "cCompile" (fun [@jsdoc {|Compiles to C|}] ~program ~optimize ->
      wrap (fun () -> to_lifted program optimize >+ Cgen.c) |> ret)

let _ =
  Js.export "doEval" (fun [@jsdoc {|Evaluate a GTLC program|}] ~program ->
      wrap (fun () -> to_elab program >>= eval >+ string_of_value) |> ret)

let _ =
  Js.export "substring"
    (fun
      [@jsdoc {|Like `String.substring`, but on the JSOO side|}] ~str
      ~start
      ~length
    ->
      let s = Js.to_string str in
      let start = Js.float_of_number start |> int_of_float in
      let length = Js.float_of_number length |> int_of_float in
      String.sub s start length |> Result.ok |> ret)

let _ =
  Js.export "docs"
    ((List.map make_builtin builtin_docs
     |> Array.of_list |> Js.array)
     [@jsdoc {|Documentation for builtin primitives|}])
