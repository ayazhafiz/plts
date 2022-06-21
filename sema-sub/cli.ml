open Lwt

let repl () =
  LTerm_inputrc.load () >>= fun () ->
  catch
    (fun () ->
      let state = () in
      Lazy.force LTerm.stdout >>= fun term ->
      Repl.loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return () | exn -> Lwt.fail exn)

let () = Lwt_main.run (repl ())
