open Sema_sub.Syntax
open Sema_sub.Type
open Sema_sub.Subtype

let count = 10000
let test name pred = QCheck.Test.make ~name ~count arbitrary_ty pred

let test2 name pred =
  QCheck.Test.make ~name ~count (QCheck.pair arbitrary_ty arbitrary_ty)
    (fun (t, u) -> pred t u)

let test3 name pred =
  QCheck.Test.make ~name ~count
    (QCheck.triple arbitrary_ty arbitrary_ty arbitrary_ty) (fun (t, u, v) ->
      pred t u v)

let test4 name pred =
  QCheck.Test.make ~name ~count
    (QCheck.quad arbitrary_ty arbitrary_ty arbitrary_ty arbitrary_ty)
    (fun (t, u, v, w) -> pred t u v w)

let test6 name pred =
  QCheck.Test.make ~name ~count
    (QCheck.pair
       (QCheck.triple arbitrary_ty arbitrary_ty arbitrary_ty)
       (QCheck.triple arbitrary_ty arbitrary_ty arbitrary_ty))
    (fun ((t, u, v), (w, x, y)) -> pred t u v w x y)

let mty = ty_of_syn
let ( <: ) t u = mty t <: mty u
let ( </: ) t u = Bool.not (t <: u)
let disj t u = tand t u <: never
let ( =~ ) t u = t <: u && u <: t

let () =
  Printexc.record_backtrace true;
  (* Tests taken from
     https://github.com/pnwamk/sst-tutorial/blob/c7ce1a35e3ba688ee275144941e729ff8b3b0c40/model/testing.rkt#L290-L655
     licensed as Apache 2.0, copyright pnwamk.
     Thank you pnwank! *)
  let suite =
    List.map
      (QCheck_alcotest.to_alcotest ~verbose:true)
      [
        test "t <: top" (fun ty -> ty <: any);
        test2 "t|u <: top" (fun t u -> tor t u <: any);
        test2 "(t, u) <: (any, any)" (fun t u -> prod t u <: prod any any);
        test2 "t -> u <: never -> any" (fun t u -> arrow t u <: arrow never any);
        test2 "(t, u) & int = never" (fun t u -> disj (prod t u) int);
        test2 "t -> u & int = never" (fun t u -> disj (arrow t u) int);
        test4 "(t, u) & v -> w = never" (fun t u v w ->
            disj (prod t u) (arrow v w));
        test2 "t <: t | u" (fun t u -> t <: tor t u);
        test2 "u <: t | u" (fun t u -> u <: tor t u);
        test2 "t & u <: t" (fun t u -> tand t u <: t);
        test2 "t & u <: u" (fun t u -> tand t u <: u);
        test2 "!(t | u) <: !t" (fun t u -> not @@ tor t u <: not t);
        test2 "!(t | u) <: !u" (fun t u -> not @@ tor t u <: not u);
        test2 "t <: u => (t, any) <: (u, any)" (fun t u ->
            if t <: u then prod t any <: prod u any
            else prod t any </: prod u any);
        test2 "t <: u => (any, t) <: (any, u)" (fun t u ->
            if t <: u then prod any t <: prod any u
            else prod any t </: prod any u);
        test3 "(t|u, v)&(!t, v) <: (u, v)" (fun t u v ->
            tand (prod (tor t u) v) (prod (not t) v) <: prod u v);
        test3 "(t, u|v)&(t, !u) <: (t, v)" (fun t u v ->
            tand (prod t (tor u v)) (prod (not t) u) <: prod t v);
        test6 "(t, u)|(v, w)|(x, y) <: (t|v|x, u|w|y)" (fun t u v w x y ->
            tor (prod t u) (tor (prod v w) (prod x y))
            <: prod (tor t (tor v x)) (tor u (tor w y)));
        test6 "(t, u)&(v, w)&(x, y) <: (t&v&x, u&w&y)" (fun t u v w x y ->
            tand (prod t u) (tand (prod v w) (prod x y))
            <: prod (tand t (tand v x)) (tand u (tand w y)));
        test6 "t|u|v -> w <: t -> w|x|y" (fun t u v w x y ->
            arrow (tor t (tor u v)) w <: arrow t (tor w (tor x y)));
        test6 "(t->u)&(v->w)&(x->y) <: t->u" (fun t u v w x y ->
            tand (arrow t u) (tand (arrow v w) (arrow x y)) <: arrow t u);
        test6 "(t->u)&(v->w)&(x->y) <: t|v|x -> u|w|y" (fun t u v w x y ->
            tand (arrow t u) (tand (arrow v w) (arrow x y))
            <: arrow (tor t (tor v x)) (tor u (tor w y)));
        test6 "(t->u)&(t|v->w)&(x->y) <: t -> u|w" (fun t u v w x y ->
            tand (arrow t u) (tand (arrow (tor t v) w) (arrow x y))
            <: arrow t (tor u w));
        (*                    *)
        (* logical properties *)
        (*                    *)
        test "t ~= t" (fun t -> t =~ t);
        test2 "t&u ~= u&t" (fun t u -> tand t u =~ tand u t);
        test3 "t&(u&v) ~= (t&u)&v" (fun t u v ->
            tand t (tand u v) =~ tand (tand t u) v);
        test2 "t|u ~= u|t" (fun t u -> tor t u =~ tor u t);
        test3 "t|(u|v) ~= (t|u)|v" (fun t u v ->
            tor t (tor u v) =~ tor (tor t u) v);
        test3 "t&(u|v) ~= t|u & t|v" (fun t u v ->
            tand t (tor u v) =~ tor (tand t u) (tand t v));
        test "t|t ~= t" (fun t -> tor t t =~ t);
        test "t&t ~= t" (fun t -> tand t t =~ t);
        test "t|!t ~= any" (fun t -> tor t (not t) =~ any);
        test "t&!t ~= never" (fun t -> tand t (not t) =~ never);
        test "!!t ~= t" (fun t -> (not (not t)) =~ t);
        test2 "!(t|u) ~= !t & !u" (fun t u ->
            (not (tor t u)) =~ tand (not t) (not u));
        test2 "!(t&u) ~= !t | !u" (fun t u ->
            (not (tand t u)) =~ tor (not t) (not u));
        test2 "(t|u)&!t <: u" (fun t u -> tand (tor t u) (not t) <: u);
        test2 "(t|u)&!u <: t" (fun t u -> tand (tor t u) (not u) <: t);
      ]
  in
  Alcotest.run ~show_errors:true "subtype" [ ("suite", suite) ]
