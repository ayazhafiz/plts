open Format
open Util
open Util.Error
open Language
open Typecheck
open Eval

let searchpath = ref [ "" ]

let argDefs =
  [
    ( "-I",
      Arg.String (fun f -> searchpath := f :: !searchpath),
      "Append a directory to the search path" );
  ]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
    (fun s ->
      match !inFile with
      | Some _ -> error Unknown "You must specify at most one input file"
      | None -> inFile := Some s)
    "";
  !inFile

let openfile infile =
  let rec trynext l =
    match l with
    | [] -> error Unknown ("Could not find " ^ infile)
    | d :: rest -> (
        let name = if d = "" then infile else d ^ "/" ^ infile in
        try open_in name with Sys_error _ -> trynext rest )
  in
  trynext !searchpath

let parse lexbuf =
  let result =
    try Some (Parser.toplevel Lexer.main lexbuf)
    with Parsing.Parse_error -> None
  in
  Parsing.clear_parser ();
  result

let parseFile inFile =
  let pi = openfile inFile in
  let lexbuf = Lexer.create inFile pi in
  let result =
    match parse lexbuf with
    | Some res -> res
    | _ -> error (Lexer.info lexbuf) "Parse error"
  in
  close_in pi;
  result

let alreadyImported = ref ([] : string list)

let checkbinding fi (ctx, nextuvar, constr) b =
  match b with
  | NameBinding -> (NameBinding, nextuvar, constr)
  | VarBinding tyT -> (VarBinding tyT, nextuvar, constr)
  | TmAbbBinding (t, None) ->
      (* Time to force a type! *)
      let tyBinding, nextuvar, constrBinding = tyRecon ctx nextuvar t in
      let allConstr = combineConstr constr constrBinding in
      let solvedSubst = unify fi ctx "Failed to unify" allConstr in
      let solvedTyT = applyTySubsts solvedSubst tyBinding in
      (TmAbbBinding (t, Some solvedTyT), nextuvar, subst2Constr solvedSubst)
  | TmAbbBinding (t, Some tyT) ->
      let tyT' = typeof ctx t in
      if tyeq ctx tyT' tyT then (TmAbbBinding (t, Some tyT), nextuvar, constr)
      else error fi "Type of binding does not match declared type"
  | TyVarBinding -> (TyVarBinding, nextuvar, constr)
  | TyAbbBinding tyT -> (TyAbbBinding tyT, nextuvar, constr)

let prbindingty ctx b =
  match b with
  | NameBinding -> ()
  | VarBinding tyT ->
      pr ": ";
      printty ctx tyT
  | TmAbbBinding (t, tyT_opt) -> (
      pr ": ";
      match tyT_opt with
      | None -> printty ctx (typeof ctx t)
      | Some tyT -> printty ctx tyT )
  | TyVarBinding -> ()
  | TyAbbBinding ty ->
      pr ":: ";
      printty ctx ty

let rec process_command (ctx, nextuvar, existingConstr, store) cmd =
  match cmd with
  | Eval (info, t) ->
      let tyT, nextuvar, constrT = tyRecon ctx nextuvar t in
      let allConstr = combineConstr existingConstr constrT in
      let solvedSubst = unify info ctx "Failed to unify" allConstr in
      let solvedTyT = applyTySubsts solvedSubst tyT in
      let t', store' = eval ctx store t in
      printtm ctx t';
      pr ": ";
      printty ctx solvedTyT;
      force_newline ();
      (ctx, nextuvar, subst2Constr solvedSubst, store')
  | Bind (fi, x, bind) ->
      let bind, nextuvar, allConstr =
        checkbinding fi (ctx, nextuvar, existingConstr) bind
      in
      let bind', store' = evalbinding ctx store bind in
      pr x;
      pr " ";
      prbindingty ctx bind';
      force_newline ();
      let ctx' = addbinding ctx x bind' in
      (* Values in the store must be updated to reflect the insertion of the new binding.
         This is not necessary during evaluation, as in that case there are no
         free variables being added to the global scope, but there are during an
         arbitrary bind command. *)
      let store' = shift_store store' 1 in
      (ctx', nextuvar, allConstr, store')
  | Load (_, file) -> process_file file (ctx, nextuvar, existingConstr, store)

and do_command (ctx, nextuvar, constr, store) c =
  open_hvbox 0;
  let state = process_command (ctx, nextuvar, constr, store) c in
  print_flush ();
  state

and process_file f (ctx, nextuvar, constr, store) =
  alreadyImported := f :: !alreadyImported;
  let cmds, _ = parseFile f ctx in
  List.fold_left do_command (ctx, nextuvar, constr, store) cmds

let readin buf =
  try
    while true do
      buf := read_line () :: !buf
    done
  with End_of_file -> pr "\n"

let rec repl (ctx, nextuvar, constr, store) =
  pr "> ";
  let buf = ref [] in
  readin buf;
  let input = String.concat "\n" !buf in
  let parsed = try parse (Lexing.from_string input) with Exit _ -> None in
  match parsed with
  | Some res ->
      let newstate =
        try
          let cmds, _ = res ctx in
          List.fold_left do_command (ctx, nextuvar, constr, store) cmds
        with Exit _ -> (ctx, nextuvar, constr, store)
      in
      pr "\n";
      repl newstate
  | _ ->
      pr "@@@ Could not parse input! @@@\n";
      repl (ctx, nextuvar, constr, store)

let main () =
  match parseArgs () with
  | Some inFile ->
      let _ =
        process_file inFile (emptycontext, uvargen, emptyconstr, emptystore)
      in
      ()
  | None -> repl (emptycontext, uvargen, emptyconstr, emptystore)

let () = set_max_boxes 1000

let () = set_margin 67

let res =
  Printexc.record_backtrace true;
  Printexc.catch
    (fun () ->
      try
        main ();
        0
      with _ ->
        Printexc.print_backtrace stdout;
        1)
    ()

let () = print_flush ()

let () = exit res
