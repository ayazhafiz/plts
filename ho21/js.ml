open Ho21
open Js_of_ocaml

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

let ( >>= ) r f = Result.map f r

let _ =
  Js.export "judge" (fun ~queries ~print_debug ~prettify_symbols ->
      wrap (fun () ->
          let queries = Js.to_string queries in
          parse_queries queries >>= List.map judge_query
          >>= List.map
                (string_of_judgement (Js.to_bool print_debug)
                   (Js.to_bool prettify_symbols))
          >>= String.concat "\n")
      |> ret)

let _ =
  Js.export "formatQueries" (fun ~queries ~prettify_symbols ->
      let queries = Js.to_string queries in
      let result =
        match parse_queries queries with
        | Ok qs ->
            List.map (string_of_query (Js.to_bool prettify_symbols)) qs
            |> String.concat "\n"
        | Error _ -> queries
      in
      Js.string result)
