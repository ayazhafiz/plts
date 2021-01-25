open Lexing
open SimpleSub.Lib

(* Repl + File reader *)

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  let result =
    try Some (SimpleSub.Parser.program SimpleSub.Lexer.read lexbuf) with
    | SimpleSub.Lexer.SyntaxError msg ->
        Printf.eprintf "Syntax error: %s at %s" msg (string_of_position lexbuf);
        None
    | _ ->
        Printf.eprintf "Parse error at %s" (string_of_position lexbuf);
        None
  in
  Parsing.clear_parser ();
  result

let ends_with s1 s2 =
  let n1 = String.length s1 and n2 = String.length s2 in
  n1 >= n2 && String.sub s1 (n1 - n2) n2 = s2

let readin buf =
  try
    while true do
      let line = read_line () in
      if ends_with line ";;" then (
        let trim = String.sub line 0 (String.length line - 2) in
        buf := !buf @ [ trim ];
        raise End_of_file )
      else buf := !buf @ [ line ]
    done
  with End_of_file -> Printf.printf "\n"

let process_program ctx toplevels =
  let bindings, ctx = pipeline_typecheck ctx toplevels in
  let results = pp_results bindings in
  Printf.printf "%s" results;
  ctx

let rec repl ctx =
  Printf.printf "> ";
  flush_all ();
  let buf = ref [] in
  readin buf;
  let input = String.concat "\n" !buf in
  let parsed = parse (Lexing.from_string ~with_positions:true input) in
  let ctx = Option.fold ~none:ctx ~some:(process_program ctx) parsed in
  flush_all ();
  Printf.printf "\n\n";
  flush_all ();
  repl ctx

let searchpath = ref [ "" ]

let openfile inFile =
  let rec trynext l =
    match l with
    | [] -> failwith ("Could not find " ^ inFile)
    | d :: rest -> (
        let name = if d = "" then inFile else d ^ "/" ^ inFile in
        try open_in name with Sys_error _ -> trynext rest )
  in
  trynext !searchpath

let process_file inFile ctx =
  let fi = openfile inFile in
  let parsed = parse (Lexing.from_channel ~with_positions:true fi) in
  let ctx = Option.fold ~none:ctx ~some:(process_program ctx) parsed in
  flush_all ();
  ctx

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
      | Some _ -> raise (Arg.Bad "You must specify at most one input file")
      | None -> inFile := Some s)
    "";
  !inFile

let main () =
  match parseArgs () with
  | Some inFile ->
      let _ = process_file inFile default_ctx in
      ()
  | None -> repl default_ctx

let () = main ()
