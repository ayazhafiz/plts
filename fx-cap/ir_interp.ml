open Ir

type value = Bool of bool | Int of int | Lam of (value -> value)
type typed_value = ty * value

let rec eval venv (_, e) =
  match e with
  | Var x -> List.assoc x venv
  | Lit (`Bool b) -> Bool b
  | Lit (`Int n) -> Int n
  | App (f, a) -> (
      let a = eval venv a in
      let f = eval venv f in
      match f with Lam f -> f a | _ -> failwith "non-applicable head")
  | Abs ((_, x), e) ->
      let lam xval =
        let venv = (x, xval) :: venv in
        eval venv e
      in
      Lam lam
  | Let ((_, x), e, r) ->
      let e = eval venv e in
      eval ((x, e) :: venv) r
  | If (c, e1, e2) -> (
      match eval venv c with
      | Bool true -> eval venv e1
      | Bool false -> eval venv e2
      | _ -> failwith "non-boolean condition")

let interp e = (fst e, eval [] e)

let readback f =
  let open Format in
  function
  | Bool b -> pp_print_bool f b
  | Int n -> pp_print_int f n
  | Lam _ -> fprintf f "<function>"

let string_of_value ?(width = Util.default_width) ((ty, value) : typed_value) =
  let open Format in
  Util.with_buffer
    (fun f ->
      fprintf f "@[<hov 2>";
      readback f value;
      fprintf f "@ : ";
      Ir.pp_ty f ty;
      fprintf f "@]")
    width
