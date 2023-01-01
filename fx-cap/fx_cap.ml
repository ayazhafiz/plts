open Lib_fx_cap

let read_chan chan =
  let lines = ref [] in
  (try
     while true do
       lines := input_line chan :: !lines
     done
   with End_of_file -> close_in chan);
  List.rev !lines

let read_file f = read_chan @@ open_in f
let read_stdin () = read_chan stdin

type input = File of string | Stdin

let input_lines = function
  | Stdin -> (None, read_stdin ())
  | File f -> (Some f, read_file f)

let infiles = ref []
let inplace = ref false
let handle_anon arg = infiles := arg :: !infiles

let parse_args () =
  Arg.parse
    [
      ("-i", Arg.Set inplace, "Write output in-place. Not relevant for stdin.");
    ]
    handle_anon ""

let main () =
  parse_args ();
  let inputs =
    match !infiles with
    | [] -> [ Stdin ]
    | files -> List.map (fun f -> File f) files
  in
  let do1 input_source =
    let opt_file, input_lines = input_lines input_source in
    let { raw_program; program; commands; file } =
      preprocess opt_file @@ raw_program_of_lines input_lines
    in
    let commands =
      List.filter_map
        (function
          | Error e -> failwith @@ string_of_command_err e | Ok e -> Some e)
        commands
    in
    let cmd_out =
      List.combine commands @@ List.map (process_one file program) commands
    in
    let cmd_out =
      List.filter_map
        (function
          | _, Error e -> failwith @@ string_of_compile_err e
          | cmd, Ok out -> Some (cmd, out))
        cmd_out
    in
    let output = postprocess raw_program cmd_out in
    match (!inplace, input_source) with
    | _, Stdin -> print_endline output
    | false, File f ->
        print_string ("# " ^ f ^ "\n");
        print_endline output
    | true, File file ->
        let chan = open_out file in
        output_string chan output;
        close_out chan
  in
  List.iter do1 inputs

let () =
  try main ()
  with Failure msg ->
    prerr_endline ("Error: " ^ msg);
    flush_all ()
