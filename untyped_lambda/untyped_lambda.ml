module UntypedLambda = Zoo.Main (struct
  let name = "untyped_lambda"

  type command = Input.toplevel

  type environment = Context.context

  let options = []

  let initial_environment = Context.empty_context

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.commandline Lexer.token)

  let eager = ref false

  let deep = ref false

  let help_text =
    "Toplevel directives:\n\
     <expr> ;                      evaluate <expr>\n\
     :lazy ;                       evaluate lazily (do not evaluate arguments)\n\
     :eager ;                      evaluate eagrly (evaluate arguments \
     immediately)\n\
     :deep ;                       evaluate inside λ-abstraction\n\
     :shallow ;                    do not evaluate inside λ-abstraction\n\
     :constant x ... y ;           declare constants\n\
     :context ;                    print current definitions\n\
     :help ;                       print this help\n\
     :quit ;                       exit\n\n\n\
     Syntax:\n\
     ^ x ... y . e                  λ-abstraction\n\
     e1 e2                          application\n"

  let exec ctx { Zoo.data = cmd; loc } =
    match cmd with
    | Input.Expr e ->
        let e = Desugar.expr ctx.Context.names e in
        let e = Eval.eval ~eager:!eager ~deep:!deep ctx.Context.decls e in
        Format.printf "%t@." (Print.expr ctx.Context.names e);
        ctx
    | Input.Context ->
        ignore
          (List.fold_right
             (fun x k ->
               ( match Context.lookup_definition k ctx with
               | None -> Format.printf "#constant @[%s@];@." x
               | Some e ->
                   Format.printf "@[%s := %t@];@." x
                     (Print.expr ctx.Context.names e) );
               k - 1)
             ctx.Context.names
             (List.length ctx.Context.names - 1));
        ctx
    | Input.Eager b ->
        eager := b;
        ctx
    | Input.Deep b ->
        deep := b;
        ctx
    | Input.TopConstant x ->
        if List.mem x ctx.Context.names then
          Zoo.error ~loc "%s already exists" x;
        Format.printf "%s is a constant.@." x;
        Context.add_parameter x ctx
    | Input.TopDefine (x, e) ->
        if List.mem x ctx.Context.names then
          Zoo.error ~loc "%s already exists" x;
        let e = Desugar.expr ctx.Context.names e in
        Format.printf "%s is defined.@." x;
        Context.add_definition x e ctx
    | Input.Help ->
        print_endline help_text;
        ctx
    | Input.Quit -> exit 0
end)

;;
UntypedLambda.main ()
