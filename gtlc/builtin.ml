open Language

type builtin = { name : string; ty : ty; doc : string }

let builtins =
  [
    { name = "succ"; ty = TArrow (TNat, TNat); doc = "`succ n` is `n + 1`" };
    { name = "pred"; ty = TArrow (TNat, TNat); doc = "`pred n` is `n - 1`" };
    {
      name = "add";
      ty = TArrow (TNat, TArrow (TNat, TNat));
      doc = "`add m n` is `m + n`";
    };
    {
      name = "mult";
      ty = TArrow (TNat, TArrow (TNat, TNat));
      doc = "`mult m n` is `m * n`";
    };
    {
      name = "eqn";
      ty = TArrow (TNat, TArrow (TNat, TBool));
      doc = "`eqn m n` is `m == n`";
    };
    {
      name = "eqb";
      ty = TArrow (TBool, TArrow (TBool, TBool));
      doc = "`eqb b c` is `b == c`";
    };
  ]

let doc_of_builtin b = (List.find (fun { name; _ } -> name = b) builtins).doc
