(** Stack-based virtual machine for lambda calculus *)

open Ast
open Util
open Symbol

(** VM instructions *)
type instruction =
  | PUSH_VAR of int  (** Push variable from environment at index *)
  | PUSH_FUN of int  (** Push function with n instructions *)
  | APPLY  (** Apply function to argument *)
  | RETURN  (** Return from function *)
  | ACCESS of int  (** Access closure environment at index *)
  | CLOSURE of int  (** Create closure with n free variables *)
  | SLIDE of int  (** Remove n elements below the top of stack *)
  | GRAB  (** Grab argument for partial application *)
  | HALT  (** Halt execution *)
[@@deriving show]

(** VM values *)
type value =
  | VClosure of value list * instruction list
      (** Closure with environment and code *)
  | VNeutral  (** Neutral value (for debugging) *)
[@@deriving show]

type state = {
  code : instruction list;  (** Current code *)
  stack : value list;  (** Value stack *)
  env : value list;  (** Current environment *)
}
(** VM state *)

let init_state code = { code; stack = []; env = [] }

let pp_instruction f = function
  | PUSH_VAR n -> Format.fprintf f "PUSH_VAR %d" n
  | PUSH_FUN n -> Format.fprintf f "PUSH_FUN %d" n
  | APPLY -> Format.fprintf f "APPLY"
  | RETURN -> Format.fprintf f "RETURN"
  | ACCESS n -> Format.fprintf f "ACCESS %d" n
  | CLOSURE n -> Format.fprintf f "CLOSURE %d" n
  | SLIDE n -> Format.fprintf f "SLIDE %d" n
  | GRAB -> Format.fprintf f "GRAB"
  | HALT -> Format.fprintf f "HALT"

let pp_instructions f instrs =
  let open Format in
  fprintf f "@[<v 0>";
  List.iteri (fun i instr -> fprintf f "%d: %a@," i pp_instruction instr) instrs;
  fprintf f "@]"

let string_of_instructions ?(width = default_width) instrs =
  with_buffer (fun f -> pp_instructions f instrs) width

let pp_value f = function
  | VClosure (env, code) ->
      Format.fprintf f "VClosure([%d env vars], [%d instrs])" (List.length env)
        (List.length code)
  | VNeutral -> Format.fprintf f "VNeutral"

let string_of_value ?(width = default_width) value =
  with_buffer (fun f -> pp_value f value) width

let compile (program : program) : instruction list =
  let rec compile_expr env (_, _, expr) =
    match expr with
    | Var x ->
        (* Find the variable in the environment *)
        let rec find_var env_list x idx =
          match env_list with
          | [] -> failwith ("Unbound variable: " ^ pp_symbol x)
          | y :: rest -> if y = x then idx else find_var rest x (idx + 1)
        in
        [ ACCESS (find_var env x 0) ]
    | Abs (x, body) ->
        (* Compile the body with the parameter added to the environment *)
        let body_code = compile_expr (x :: env) body in
        [ GRAB ] @ body_code
    | App (f, arg) ->
        (* Compile function and argument, then apply *)
        let f_code = compile_expr env f in
        let arg_code = compile_expr env arg in
        f_code @ arg_code @ [ APPLY ]
  in
  let main_code = compile_expr [] program in
  main_code @ [ HALT ]

let compile_program (program : program) : instruction list = compile program
