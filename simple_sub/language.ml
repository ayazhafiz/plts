type term =
  | Num of int
  | Var of string
  | Abs of string * term
  | App of term * term
  | Record of (string * term) list
  | RecordProject of term * string
  | Let of { is_rec : bool; name : string; rhs : term; body : term }

type var_state = {
  uid : int;
  level : int;
      (** The level at which the variable should be generalized.
          Useful in representing let polymorphism, which can be deeply nested in
          many "let" terms, each term being more general than the last. *)
  mutable lower_bounds : simple_ty list;
  mutable upper_bounds : simple_ty list;
}

(** Types inferred from the frontend *)
and simple_ty =
  | STyVar of var_state
  | STyInt
  | STyFn of simple_ty * simple_ty
  | STyRecord of (string * simple_ty) list

type poly_ty = PolyTy of int * simple_ty

(** Types after inference and constraining of simple types *)
type ty =
  | TyTop
  | TyBottom
  | TyUnion of ty * ty
  | TyIntersection of ty * ty
  | TyFn of ty * ty
  | TyRecord of (string * ty) list
  | TyRecursive of string * ty
  | TyVar of string
  | TyInt

type polar_var =
  | Positive of var_state  (** Positive variables are in output positions *)
  | Negative of var_state  (** Negative variables are in input positions *)
