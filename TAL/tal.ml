let lex = Lexing.from_string ~with_positions:true

let parse_term s = lex s |> Parser.toplevel_term Lexer.read

module type Lang = sig
  type term

  type ty

  val string_of_term : term -> string

  val string_of_ty : ty -> string
end

module F = struct
  include F

  type elaborated_term = term

  let typeof = function Annot (_, t) -> t | _ -> failwith "not elaborated"
end

module K = struct
  include K

  let check_well_typed = term_wf [] []

  let of_F = trans_top
end

module C = struct
  include C

  let check_well_typed = term_wf [] []

  let of_K = trans_top
end

module H = struct
  include H

  type term = program

  let string_of_term = string_of_program

  let eval = eval_top

  let check_well_typed = check_program

  let of_C = trans_top
end
