open Format
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
      | Some _ -> failwith "You must specify at most one input file"
      | None -> inFile := Some s)
    "";
  !inFile

let openfile infile =
  let rec trynext l =
    match l with
    | [] -> failwith ("Could not find " ^ infile)
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
    match parse lexbuf with Some res -> res | _ -> failwith "Parse error"
  in
  close_in pi;
  result

let process_command (ctx, store) cmd =
  match cmd with
  | Eval (_, t) ->
      let tyT = typeof ctx t in
      let t, store' = eval ctx store t in
      Printf.printf "%s: %s" (string_of_term ctx t) (string_of_ty ctx tyT);
      force_newline ();
      (ctx, store')
  | Bind (_, x, bind) ->
      Printf.printf "%s%s" x (string_of_binding ctx bind);
      force_newline ();
      let ctx' = addbinding ctx x bind in
      (* Values in the store must be updated to reflect the insertion of the new binding.
         This is not necessary during evaluation, as in that case there are no
         free variables being added to the global scope, but there are during an
         arbitrary bind command. *)
      let store' = shift_store store 1 in
      (ctx', store')

let do_command (ctx, store) c =
  open_hvbox 0;
  let state = process_command (ctx, store) c in
  flush_all ();
  print_flush ();
  state

let pr_cmd ctx = function
  | Eval (_, t) ->
      Printf.printf "EVAL:~: %s\n" (string_of_term ctx t);
      flush_all ()
  | Bind (_, n, t) ->
      Printf.printf "BIND:~: %s ## %s" n (string_of_binding ctx t);
      flush_all ()

let process_file f (ctx, store) =
  let cmds, _ = parseFile f ctx in
  List.fold_left do_command (ctx, store) cmds

let readin buf =
  try
    while true do
      buf := read_line () :: !buf
    done
  with End_of_file -> force_newline ()

let rec repl (ctx, store) =
  Printf.printf "> ";
  let buf = ref [] in
  readin buf;
  let input = String.concat "\n" !buf in
  let newstate =
    match parse (Lexing.from_string input) with
    | Some res -> (
        try
          let cmds, _ = res ctx in
          List.fold_left do_command (ctx, store) cmds
        with Failure f ->
          Printf.eprintf "%s\n" f;
          (ctx, store) )
    | _ -> (ctx, store)
  in
  flush_all ();
  Printf.printf "\n\n";
  flush_all ();
  repl newstate

let main () =
  match parseArgs () with
  | Some inFile ->
      let _ = process_file inFile (emptycontext, emptystore) in
      ()
  | None -> repl (emptycontext, emptystore)

let () = set_max_boxes 1000

let () = set_margin 67

let res =
  Printexc.catch
    (fun () ->
      try
        main ();
        0
      with Failure s ->
        Printf.eprintf "Fatal error: %s" s;
        1)
    ()

let () = print_flush ()

let () = exit res
