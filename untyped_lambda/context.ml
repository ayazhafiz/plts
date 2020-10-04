(** Program context. *)

type declaration = Grammar.term option
(** Declaration of a parameter or a definition *)

type context = { names : string list; decls : declaration list }

let empty_context = { names = []; decls = [] }

(** Looks up the definition of [k] in a context. *)
let lookup_definition k { decls = lst; _ } =
  match List.nth lst k with
  | Some e -> Some (Grammar.shift (k + 1) e)
  | None -> None

(** Adds the parameter [x] to the context [ctx], returning [ctx]. *)
let add_parameter x ctx = { names = x :: ctx.names; decls = None :: ctx.decls }

(** Adds the [x] defined as [e] to the context [ctx], returning [ctx]. *)
let add_definition x e ctx =
  { names = x :: ctx.names; decls = Some e :: ctx.decls }
