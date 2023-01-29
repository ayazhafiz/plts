open Ir

type value =
  | Bool of bool
  | Int of int
  | Lam of (value -> value)
  | RefVal of value ref

type typed_value = ty * value

let value_of_builtin : Ast.builtin -> value =
  let on_two_ints f =
    Lam
      (fun v1 ->
        Lam
          (fun v2 ->
            match (v1, v2) with
            | Int m, Int n -> f m n
            | _ -> failwith "not two ints"))
  in
  function
  | `Lt -> on_two_ints (fun m n -> Bool (m < n))
  | `Gt -> on_two_ints (fun m n -> Bool (m > n))
  | `Add -> on_two_ints (fun m n -> Int (m + n))
  | `Sub -> on_two_ints (fun m n -> Int (m - n))
  | `Mul -> on_two_ints (fun m n -> Int (m * n))

let rec chase = function RefVal v -> chase !v | v -> v

let rec eval venv (_, e) =
  match e with
  | Var x -> List.assoc x venv
  | Builtin b -> value_of_builtin b
  | Lit (`Bool b) -> Bool b
  | Lit (`Int n) -> Int n
  | App (f, a) -> (
      let a = eval venv a in
      let f = eval venv f in
      match chase f with Lam f -> f a | _ -> failwith "non-applicable head")
  | Abs ((_, x), e) ->
      let lam xval =
        let venv = (x, xval) :: venv in
        eval venv e
      in
      Lam lam
  | Let (`Rec false, (_, x), e, r) ->
      let e = eval venv e in
      eval ((x, e) :: venv) r
  | Let (`Rec true, (_, x), e, r) ->
      let rec_cell = ref @@ Lam (fun _ -> failwith "unimplemented") in
      let e = eval ((x, RefVal rec_cell) :: venv) e in
      rec_cell := e;
      eval ((x, e) :: venv) r
  | If (c, e1, e2) -> (
      match chase @@ eval venv c with
      | Bool true -> eval venv e1
      | Bool false -> eval venv e2
      | _ -> failwith "non-boolean condition")

let interp e = (fst e, eval [] e)

let rec readback f =
  let open Format in
  function
  | Bool b -> pp_print_bool f b
  | Int n -> pp_print_int f n
  | Lam _ -> fprintf f "<function>"
  | RefVal v -> readback f !v

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
