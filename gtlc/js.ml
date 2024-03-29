open Gtlc
open Js_of_ocaml

let ( |>= ) = Option.bind
let ( >>= ) = Result.bind
let ( >+ ) v f = Result.map f v

let to_infer program =
  Js.to_string program |> parse >>= fun (e, ft) -> infer e ft

let to_elab program = to_infer program >>= elaborate

let to_lifted program opt =
  to_elab program >+ insert_casts >+ lift ~optimize:(Js.to_bool opt)

let n num = Js.float_of_number num |> int_of_float

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

let js_pos (line, col) =
  object%js
    val line = Js.number_of_float (float_of_int line)
    val col = Js.number_of_float (float_of_int col)
  end

let js_range { start_pos; end_pos } =
  object%js
    val startPos = js_pos start_pos
    val endPos = js_pos end_pos
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
  Js.export "infer"
    (fun [@jsdoc {|Infers marked type variables|}] ~program ~width ->
      wrap (fun () -> to_infer program >+ string_of_expr ~width:(n width))
      |> ret)

let _ =
  Js.export "irCompile"
    (fun [@jsdoc {|Compiles to compiler-internal IR|}] ~program ~optimize ~width
    ->
      wrap (fun () ->
          to_lifted program optimize
          >+ string_of_lifted_program ~width:(n width))
      |> ret)

let _ =
  Js.export "tsCompile"
    (fun [@jsdoc {|Compiles to TypeScript|}] ~program ~optimize ~width ->
      wrap (fun () ->
          to_lifted program optimize >+ Cgen.typescript ~width:(n width))
      |> ret)

let _ =
  Js.export "cCompile"
    (fun [@jsdoc {|Compiles to C|}] ~program ~optimize ~width ->
      wrap (fun () -> to_lifted program optimize >+ Cgen.c ~width:(n width))
      |> ret)

let _ =
  Js.export "doEval"
    (fun [@jsdoc {|Evaluate a GTLC program|}] ~program ~width ->
      wrap (fun () ->
          to_elab program >>= eval >+ string_of_value ~width:(n width))
      |> ret)

let _ =
  Js.export "docs"
    ((List.map make_builtin builtin_docs
     |> Array.of_list |> Js.array)
     [@jsdoc {|Documentation for builtin primitives|}])

let _ =
  Js.export "getHover"
    (fun [@jsdoc {|Get hover information|}] ~program ~line ~column ->
      let line, col = (n line, n column) in
      wrap (fun () ->
          to_elab program |> Result.to_option |>= get_hover ~line ~col
          |> Option.to_result ~none:"")
      |> function
      | Ok (info, range) ->
          Js.(
            some
            @@ object%js
                 val info =
                   info |> Array.of_list |> Array.map string |> Js.array

                 val range = js_range range
               end)
      | Error _ -> Js.null)
