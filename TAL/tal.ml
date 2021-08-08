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
end

module K = struct
  include K

  let check_well_typed = term_wf [] []

  let of_F = trans_top
end
