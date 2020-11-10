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

let checkbinding fi ctx b =
  match b with
  | NameBinding -> NameBinding
  | VarBinding tyT -> VarBinding tyT
  | TmAbbBinding (t, None) -> TmAbbBinding (t, Some (typeof ctx t))
  | TmAbbBinding (t, Some tyT) ->
      let tyT' = typeof ctx t in
      if tyeq ctx tyT' tyT then TmAbbBinding (t, Some tyT)
      else error fi "Type of binding does not match declared type"
  | TyVarBinding -> TyVarBinding
  | TyAbbBinding tyT -> TyAbbBinding tyT

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

let process_command ctx cmd =
  match cmd with
  | Eval (_, t) ->
      let tyT = typeof ctx t in
      let t' = eval ctx t in
      printtm ctx t';
      pr ": ";
      printty ctx tyT;
      force_newline ();
      ctx
  | Bind (fi, x, bind) ->
      let bind = checkbinding fi ctx bind in
      let bind' = evalbinding ctx bind in
      pr x;
      pr " ";
      prbindingty ctx bind';
      force_newline ();
      addbinding ctx x bind'

let do_command ctx c =
  open_hvbox 0;
  let newctx = process_command ctx c in
  print_flush ();
  newctx

let process_file f ctx =
  alreadyImported := f :: !alreadyImported;
  let cmds, _ = parseFile f ctx in
  List.fold_left do_command ctx cmds

let readin buf =
  try
    while true do
      buf := read_line () :: !buf
    done
  with End_of_file -> pr "\n"

let rec repl ctx =
  pr "> ";
  let buf = ref [] in
  readin buf;
  let input = String.concat "\n" !buf in
  match parse (Lexing.from_string input) with
  | Some res ->
      let cmds, _ = res ctx in
      let newctx = List.fold_left do_command ctx cmds in
      pr "\n";
      repl newctx
  | _ ->
      pr "@@@ Could not parse input! @@@\n";
      repl ctx

let main () =
  match parseArgs () with
  | Some inFile ->
      let _ = process_file inFile emptycontext in
      ()
  | None -> repl emptycontext

let () = set_max_boxes 1000

let () = set_margin 67

let res =
  Printexc.catch
    (fun () ->
      try
        main ();
        0
      with Exit x -> x)
    ()

let () = print_flush ()

let () = exit res
