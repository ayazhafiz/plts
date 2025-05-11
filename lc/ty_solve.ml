open Ast

(** Simple type inference for lambda calculus *)

module T = struct
  let fresh_var_counter = ref 0
  
  let fresh_var () =
    incr fresh_var_counter;
    TVar !fresh_var_counter
end
