module rec Ast : sig
  type ty =
    | Any
    | Never
    | Int
    | Tuple of ty list
    | Not of ty
    | Inter of TySet.t
    | Union of TySet.t

  type term =
    | Num of int
    | Var of string * ty option ref
    | Tup of term list * ty option ref
    | App of string * term list * ty option ref
    | Dec of string * (string * ty) list * term * term * ty option ref
    | If of string * ty * term * term * ty option ref
end =
  Ast

and TySet : sig
  include Set.S with type elt = Ast.ty

  val to_list : t -> elt list
end = struct
  include Set.Make (struct
    type t = Ast.ty

    let compare = compare
  end)

  let to_list t = to_seq t |> List.of_seq
end

open Ast

let with_formatter cb =
  let open Format in
  let buf = Buffer.create 32 in
  let f = formatter_of_buffer buf in
  cb f;
  pp_print_flush f ();
  Buffer.to_seq buf |> String.of_seq

let print_sep f print_item sep lst =
  let lasti = List.length lst - 1 in
  List.iteri
    (fun i item ->
      print_item item;
      if i <> lasti then Format.fprintf f sep)
    lst

let should_wrap =
  let prec = function
    | Union _ -> 1
    | Inter _ -> 2
    | Not _ -> 3
    | _ -> failwith "unreachable"
  in
  function
  | (Any | Never | Int | Tuple _), _ | _, (Any | Never | Int | Tuple _) -> false
  | child, parent -> prec child < prec parent

let print_ty f ty =
  let open Format in
  let rec s outer_break parent_ty ty =
    let wrap = should_wrap (ty, parent_ty) in
    let inter_in_union =
      match (ty, parent_ty) with Inter _, Union _ -> true | _ -> false
    in
    if wrap then fprintf f "(";
    (match ty with
    | Any -> pp_print_string f "any"
    | Never -> pp_print_string f "never"
    | Int -> pp_print_string f "int"
    | Tuple tys ->
        fprintf f "(";
        print_sep f (s true ty) (if outer_break then ", " else ",@ ") tys;
        fprintf f ")"
    | Not ty1 ->
        fprintf f "!";
        s outer_break ty ty1
    | Inter tys ->
        print_sep f (s true ty)
          (if inter_in_union then "&"
          else if outer_break then " & "
          else " &@ ")
          (TySet.to_list tys)
    | Union tys ->
        print_sep f (s true ty)
          (if outer_break then " | " else " |@ ")
          (TySet.to_list tys));
    if wrap then fprintf f ")"
  in
  s false Never ty

let string_of_ty ty =
  let open Format in
  with_formatter (fun f ->
      pp_open_box f 2;
      print_ty f ty;
      pp_close_box f ())

let print_term f t =
  let open Format in
  let rec s = function
    | Num i -> pp_print_int f i
    | Var (s, _) -> pp_print_string f s
    | Tup (ts, _) ->
        fprintf f "(";
        print_sep f s ",@ " ts;
        fprintf f ")"
    | App (fn, ts, _) ->
        fprintf f "%s " fn;
        print_sep f s "@ " ts
    | Dec (fn, vars, body, cont, _) ->
        fprintf f "@[<v 0>@[<v 2>@[<hov 2>fn %s(" fn;
        print_sep f
          (fun (var, ty) ->
            fprintf f "%s: " var;
            print_ty f ty)
          ",@ " vars;
        fprintf f ") =@]@ ";
        s body;
        fprintf f "@]@ in@]@ ";
        s cont
    | If (var, is, then', else', _) ->
        fprintf f "@[<v>@[<hov 2>if %s is @[" var;
        print_ty f is;
        fprintf f "@]@]@ @[<v 2>then@ @[";
        s then';
        fprintf f "@]@]@ @[<v 2>else@ @[";
        s else';
        fprintf f "@]@]@]"
  in
  s t

let string_of_term t =
  let open Format in
  with_formatter (fun f ->
      pp_open_box f 2;
      print_term f t;
      pp_close_box f ())
