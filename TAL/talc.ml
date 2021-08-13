let lex = Lexing.from_string ~with_positions:true

let parse_term s = lex s |> fun s -> Parser.toplevel_term Lexer.read s []

module type Lang = sig
  type term

  type ty

  val string_of_term : term -> string

  val string_of_ty : ty -> string
end

module type Lower = sig
  include Lang

  type source

  type value

  val string_of_value : value -> string

  val convert : source -> term

  val check_well_typed : term -> unit

  val eval : term -> value
end

module F = struct
  include F

  type elaborated_term = term

  let typeof = function Annot (_, t) -> t | _ -> failwith "not elaborated"
end

module K = struct
  include K

  type source = F.elaborated_term

  let check_well_typed = term_wf [] []

  let convert = trans_top
end

module C = struct
  include C

  type source = K.term

  let check_well_typed = term_wf [] []

  let convert = trans_top
end

module H = struct
  include H

  type source = C.term

  type term = program

  let string_of_term = string_of_program

  let eval = eval_top

  let check_well_typed = check_program

  let convert = trans_top
end

module A = struct
  include A

  type source = H.term

  type term = program

  let string_of_term = string_of_program

  let eval = eval_top

  let check_well_typed = check_program

  let convert = trans_top
end

module type Int = Tal.Int

module TAL (Int : Int) = struct
  include Tal.TAL (Int)

  type value = eval_value

  let string_of_value = string_of_eval_value

  let string_of_ty = sty

  type source = A.term

  type term = program

  let string_of_term = sprog

  let eval = eval

  let check_well_typed = check_prog

  let convert = trans_prog
end

module OCamlInt = struct
  type t = int

  let ( + ) = ( + )

  let ( - ) = ( - )

  let ( * ) = ( * )

  let ( = ) = ( = )

  let string_of = string_of_int

  let of_ocaml_int = Fun.id

  let to_ocaml_int = Fun.id
end

module OCamlTAL = TAL (OCamlInt)

module X86 (Int : Int) = struct
  include X86.X86 (Int)

  type value = rt_value

  let convert = trans_prog

  let print = string_of_prog

  let print_value = string_of_rt_value
end

exception TyError = Util.TyErr

exception EvalError = Util.EvalErr
