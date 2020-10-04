module CalcVar = Zoo.Main (struct
  let name = "calc_var"

  type command = Grammar.statement

  type environment = (string * int) list

  let options = []

  let initial_environment = []

  let file_parser = None

  let toplevel_parser = Some (Parser.toplevel Lexer.lexeme)

  let exec env cmd =
    match cmd with
    | Grammar.Definition (x, e) ->
        let n = Eval.eval env e in
        (x, n) :: env
    | Grammar.Expression e ->
        let n = Eval.eval env e in
        Zoo.print_info "%d\n" n;
        env
end)

;;
CalcVar.main ()
