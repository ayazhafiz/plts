let lex = Lexing.from_string ~with_positions:true

let parse_term s = lex s |> Parser.toplevel_term Lexer.read

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

module TAL = struct
  include Tal

  type value = word_value

  let string_of_value = swv

  let string_of_ty = sty

  type source = A.term

  type term = program

  let string_of_term = sprog

  let eval = eval

  let check_well_typed = check_prog

  let convert = trans_prog
end
