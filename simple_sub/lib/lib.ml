open Language
open Typecheck
open Simplify
open Print

(* Top-level typechecking drivers *)

(** [pipeline_typecheck ctx program] infers the types for a program in a
    context, pushes the inferred typings through the simplification pipeline,
    and returns a string of new bindings in the program as well as the context
    updated with those bindings. *)
let pipeline_typecheck ctx program =
  let bindings, ctx = typeProgram ctx program in
  let bindings =
    List.map
      (fun r ->
        Result.bind r (fun (name, ty) ->
            try
              let ty =
                ( match ty with
                | `S simple -> simple
                | `P poly -> instantiate 0 poly )
                |> canonicalizeSimpleTy |> simplifyTy |> coalesceCompactTy
              in
              Ok (name, ty)
            with Failure m -> Error m))
      bindings
  in
  (bindings, ctx)

let default_ctx =
  let b = STyPrim "bool" in
  let i = STyPrim "int" in
  [
    ("true", `S b);
    ("false", `S b);
    ("not", `S (STyFn (b, b)));
    ("succ", `S (STyFn (i, i)));
    ("add", `S (STyFn (i, STyFn (i, i))));
    ( "if",
      let v = STyVar (freshVar 1) in
      `P (PolyTy (0, STyFn (b, STyFn (v, STyFn (v, v))))) );
  ]
  |> List.to_seq |> Ctx.of_seq

let pp_results results =
  results
  |> List.map (function
       | Ok (name, ty) -> Printf.sprintf "%s: %s" name (string_of_ty ty)
       | Error msg -> Printf.sprintf "Type error: %s" msg)
  |> String.concat "\n"
