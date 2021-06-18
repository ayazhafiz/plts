module F = Strictly_annotated

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

let fmt_list fmt_item sep items =
  let open F in
  let lasti = List.length items - 1 in
  List.map fmt_item items
  |> List.mapi (fun i x -> if i = lasti then x else x ^^ sep)
  |> List.fold_left ( ^^ ) empty

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

let width = 80

let cmt = "  # "

let fmt_ty ty =
  let open F in
  let rec s parent_breaks parent_ty ty =
    let wrap = should_wrap (ty, parent_ty) in
    let inter_in_union =
      match (ty, parent_ty) with Inter _, Union _ -> true | _ -> false
    in
    let fty =
      match ty with
      | Any -> text "any"
      | Never -> text "never"
      | Int -> text "int"
      | Tuple tys ->
          let sep = text "," ^^ if parent_breaks then text " " else space in
          let inner = fmt_list (s true ty) sep tys in
          group (nest 2 (text "(" ^^ inner ^^ text ")"))
      | Not ty1 -> group (text "!" ^^ s parent_breaks ty ty1)
      | Inter tys ->
          let sep =
            if inter_in_union then text "&"
            else if parent_breaks then text " & "
            else text " &" ^^ space
          in
          group (nest 2 (fmt_list (s true ty) sep (TySet.to_list tys)))
      | Union tys ->
          let sep = if parent_breaks then text " | " else text " |" ^^ space in
          group (nest 2 (fmt_list (s true ty) sep (TySet.to_list tys)))
    in
    if wrap then group (text "(" ^^ fty ^^ text ")") else fty
  in
  s false Never ty

let string_of_ty ty =
  let open F in
  pretty width cmt (fmt_ty ty)

let fmt_term t =
  let open F in
  let sty ty = string_of_ty (Option.get !ty) in
  let rec s = function
    | Num i -> text (string_of_int i)
    | Var (s, ty) -> texta s (sty ty)
    | Tup (ts, ty) ->
        let sep = text "," ^^ space in
        let inner = fmt_list s sep ts in
        group
          (text "(" ^^ group (nest 1 inner) ^^ break "" ^^ texta ")" (sty ty))
    | App (fn, ts, ty) ->
        let ty = fn ^ " .. ~> " ^ sty ty in
        let indent = String.length fn + 1 in
        group (nest indent (texta fn ty ^| fmt_list s space ts))
    | Dec (fn, formals, body, cont, ty) ->
        let ty = "=> " ^ sty ty in
        let formals =
          fmt_list
            (fun (p, ty) -> text p ^^ text ": " ^^ fmt_ty ty)
            (text "," ^^ space)
            formals
        in
        let header =
          text "fn " ^^ text fn ^^ text "(" ^^ formals ^^ texta ") =" ty
          |> nest 2 |> group
        in
        let body = s body in
        let decl = header ^| body |> nest 2 |> group in
        group (decl ^| group (nest 2 (text "in" ^| s cont)))
    | If (var, is, then', else', ty) ->
        let ty = "if .. ~> " ^ sty ty in
        group
          (group
             (nest 2 (texta "if " ty ^^ text var ^^ text " is" ^| fmt_ty is))
          ^| group (nest 2 (text "then" ^| s then'))
          ^| group (nest 2 (text "else" ^| s else')))
  in
  s t

let string_of_term t =
  let open F in
  pretty ~global_align:true width cmt (fmt_term t)
