(** Runtime evaluator for the VM *)

open Vm

(** Helper function to split a list at a specific index *)
module List = struct
  include List

  let split_at n l =
    let rec aux i acc = function
      | [] -> (List.rev acc, [])
      | h :: t as l ->
          if i = 0 then (List.rev acc, l) else aux (i - 1) (h :: acc) t
    in
    aux n [] l

  let drop n l =
    let rec aux i = function
      | [] -> []
      | _ :: t as l -> if i = 0 then l else aux (i - 1) t
    in
    aux n l
end

(** Execute a single instruction in the VM *)
let exec_instruction state =
  match state.code with
  | [] -> failwith "Empty code"
  | instr :: rest_code -> (
      match instr with
      | PUSH_VAR n ->
          (* Push variable from environment *)
          if n >= List.length state.env then
            failwith ("Variable index out of bounds: " ^ string_of_int n)
          else
            let var = List.nth state.env n in
            { state with code = rest_code; stack = var :: state.stack }
      | PUSH_FUN n ->
          (* Push function code onto stack *)
          let fun_code, rest =
            try List.split_at n rest_code
            with _ ->
              failwith
                ("Not enough instructions for PUSH_FUN: " ^ string_of_int n)
          in
          let closure = VClosure (state.env, fun_code) in
          { state with code = rest; stack = closure :: state.stack }
      | APPLY -> (
          (* Apply function to argument *)
          match state.stack with
          | arg :: VClosure (env, code) :: rest_stack ->
              { code; stack = rest_stack; env = arg :: env }
          | _ -> failwith "APPLY: Invalid stack")
      | RETURN -> (
          (* Return from function *)
          match state.stack with
          | result :: _ -> { state with code = rest_code; stack = [ result ] }
          | _ -> failwith "RETURN: Empty stack")
      | ACCESS n ->
          (* Access closure environment *)
          if n >= List.length state.env then
            failwith ("Environment index out of bounds: " ^ string_of_int n)
          else
            let var = List.nth state.env n in
            { state with code = rest_code; stack = var :: state.stack }
      | CLOSURE n ->
          (* Create closure with n free variables *)
          let rec take_n_vars n stack env =
            if n = 0 then (stack, env)
            else
              match stack with
              | v :: rest -> take_n_vars (n - 1) rest (v :: env)
              | _ -> failwith "CLOSURE: Not enough values on stack"
          in
          let code_body, rest_code =
            try List.split_at n rest_code
            with _ ->
              failwith
                ("Not enough instructions for CLOSURE: " ^ string_of_int n)
          in
          let rest_stack, closure_env = take_n_vars n state.stack [] in
          let closure = VClosure (closure_env, code_body) in
          { state with code = rest_code; stack = closure :: rest_stack }
      | SLIDE n -> (
          (* Remove n elements below the top of stack *)
          match state.stack with
          | top :: rest ->
              let new_rest =
                try List.drop n rest
                with _ ->
                  failwith
                    ("SLIDE: Not enough values on stack: " ^ string_of_int n)
              in
              { state with code = rest_code; stack = top :: new_rest }
          | _ -> failwith "SLIDE: Empty stack")
      | GRAB -> (
          (* Grab argument for partial application *)
          match state.stack with
          | arg :: rest ->
              { code = rest_code; stack = rest; env = arg :: state.env }
          | [] ->
              (* No argument available, create a closure *)
              let closure = VClosure (state.env, [ GRAB ] @ rest_code) in
              { state with code = [ HALT ]; stack = [ closure ] })
      | HALT ->
          (* Halt execution *)
          state)

(** Run the VM until it halts *)
let rec run state =
  match state.code with
  | [] -> state
  | HALT :: _ -> state
  | _ -> run (exec_instruction state)

(** Evaluate a program *)
let eval program =
  let code = compile_program program in
  let initial_state = init_state code in
  let final_state = run initial_state in
  match final_state.stack with
  | [ result ] -> result
  | [] -> failwith "Evaluation resulted in empty stack"
  | _ -> failwith "Evaluation resulted in multiple values on stack"
