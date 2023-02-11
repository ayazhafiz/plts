open Vm_layout
module T = Ty_solve.T

type debug_frame = { locals : (string * (Ast.ty * [ `FpOffset of int ])) list }

let new_debug_frame () = { locals = [] }

let rec elaborate_local (offset, (x, ty)) =
  let open Ast in
  match !(unlink ty) with
  | Link _ | Unbd _ -> failwith "unreachable"
  | Content c -> (
      match c with
      | TInt | TBool | TFn _ -> [ (offset, (x, ty)) ]
      | TTup ts ->
          let numbered_indices =
            List.rev @@ List.mapi (fun i t -> (string_of_int i, t)) ts
          in
          elaborate_struct x offset numbered_indices
      | TFiber t ->
          let indices =
            [
              ("stkdirty", T.int);
              ("stkidx", T.int);
              ("return", t);
              ("bit", T.int);
            ]
          in
          elaborate_struct x offset indices
      | TTupSparse _ -> failwith "unreachable")

and elaborate_struct x offset = function
  | [] -> []
  | (i, t) :: rest ->
      let name = Printf.sprintf "%s.%s" x i in
      let stksize = stack_size t in
      elaborate_local (offset, (name, t))
      @ elaborate_struct x (offset + stksize) rest

let fill_out (offset, (x, ty)) =
  let stksize = stack_size ty in
  match stksize with
  | 0 -> []
  | 1 -> [ (offset, Some x) ]
  | n ->
      assert (n >= 0);
      let addt_lines = List.init (n - 1) (fun m -> (offset + m + 1, None)) in
      (offset, Some x) :: addt_lines

let pp_debug_frame f { locals } =
  let locals =
    ("@old_fp", (T.int, `FpOffset (-2)))
    :: ("@old_sp", (T.int, `FpOffset (-1)))
    :: locals
  in
  let by_offset =
    List.sort compare
    @@ List.map (fun (x, (t, `FpOffset offset)) -> (offset, (x, t))) locals
  in
  let by_offset_elaborated =
    List.flatten @@ List.map elaborate_local by_offset
  in
  let filled_out = List.flatten @@ List.map fill_out by_offset_elaborated in
  let widest_n =
    max
      (String.length @@ string_of_int (fst @@ List.hd filled_out))
      (String.length @@ string_of_int (fst @@ List.hd @@ List.rev filled_out))
  in
  let open Format in
  fprintf f "@[<v 0>%% Stack relative to frame pointer:@,";
  Util.intersperse f ""
    (fun f not_first (offset, name) ->
      if not_first then fprintf f "@,";
      let offset = string_of_int offset in
      let offset = String.make (widest_n - String.length offset) ' ' ^ offset in
      let name = Option.value ~default:"" name in
      fprintf f "%%   %s  %s" offset name)
    filled_out;
  fprintf f "@]"
