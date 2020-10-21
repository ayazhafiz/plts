open Format

let info = Error.info

let pr = Format.print_string

module Error = struct
  exception Exit of int

  (** File: path, line, co *)
  type info = File of string * int * int | Unknown

  type 'a withinfo = { i : info; v : 'a }

  let dummyinfo = Unknown

  let makeinfo f l c = File (f, l, c)

  let printInfo info =
    match info with
    | File (f, l, c) ->
        print_string f;
        print_string "@";
        print_string l;
        print_string ":";
        print_string c;
        print_string ":"
    | Unknown -> print_string "<unknown>:"

  let errAt info f =
    printInfo info;
    print_space ();
    f ()

  let error info s =
    errAt info (fun () ->
        print_string s;
        print_newline ())

  let warningAt info s =
    printInfo info;
    print_string "Warning: ";
    print_string s;
    print_newline ()
end
