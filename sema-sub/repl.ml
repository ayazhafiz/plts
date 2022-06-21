open Sema_sub.Lib
open Lwt
open React
open LTerm_text
open LTerm_style

type state = unit

let repl_eval state s = (state, compile_cmd s)
let prompt _state = eval [ B_underline true; S "[sub]>"; E_underline ]

let output _state = function
  | Ok e -> eval [ B_bold true; S e; E_bold ]
  | Error e -> eval [ B_fg red; S e; E_fg ]

class read_line ~term ~history ~state =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    method! show_box = false
    initializer self#set_prompt (S.const (prompt state))
  end

let rec loop term history state =
  Lwt.catch
    (fun () ->
      let rl =
        new read_line ~term ~history:(LTerm_history.contents history) ~state
      in
      rl#run >|= fun command -> Some command)
    (function Sys.Break -> return None | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
      let command_utf8 = Zed_string.to_utf8 command in
      let state, out = repl_eval state command_utf8 in
      LTerm.fprintls term (output state out) >>= fun () ->
      LTerm_history.add history command;
      loop term history state
  | None -> loop term history state
