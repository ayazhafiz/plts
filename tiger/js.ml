open Tiger
open Js_of_ocaml

let ( >>= ) = Result.bind

let ( |~> ) r f = Result.map f r

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

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let parse s =
  let lexbuf = Lexing.from_string ~with_positions:true s in
  try Front.Parser.toplevel Front.Lexer.read lexbuf |> Result.ok with
  | Front.Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position lexbuf.lex_curr_p))
  | Front.Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position lexbuf.lex_curr_p))

let drive s o =
  s |> Js.to_string |> parse >>= fun e ->
  (Front.Semantic.check_prog e |~> fun () -> e) |~> fun e ->
  match o with
  | `Ir -> Backend_registry.X86_64_Backend.emit_ir e
  | `X86 -> Backend_registry.X86_64_Backend.emit_assem e

let _ =
  Js.export "compileIR"
    (fun
      [@jsdoc {|Compile a tiger program to the compiler-internal IR|}] ~program
    -> drive program `Ir |> ret)

let _ =
  Js.export "compileX86"
    (fun [@jsdoc {|Compile a tiger program to x86 assembly|}] ~program ->
      drive program `X86 |> ret)
