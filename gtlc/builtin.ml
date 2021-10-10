open Language

type builtin = { name : string; ty : ty; doc : string }

let builtins =
  [
    {
      name = "succ";
      ty = ft (TArrow (ft TNat, ft TNat));
      doc = "`succ n` is `n + 1`";
    };
    {
      name = "pred";
      ty = ft (TArrow (ft TNat, ft TNat));
      doc = "`pred n` is `n - 1`";
    };
    {
      name = "add";
      ty = ft (TArrow (ft TNat, ft (TArrow (ft TNat, ft TNat))));
      doc = "`add m n` is `m + n`";
    };
    {
      name = "mult";
      ty = ft (TArrow (ft TNat, ft (TArrow (ft TNat, ft TNat))));
      doc = "`mult m n` is `m * n`";
    };
    {
      name = "eqn";
      ty = ft (TArrow (ft TNat, ft (TArrow (ft TNat, ft TBool))));
      doc = "`eqn m n` is `m == n`";
    };
    {
      name = "eqb";
      ty = ft (TArrow (ft TBool, ft (TArrow (ft TBool, ft TBool))));
      doc = "`eqb b c` is `b == c`";
    };
  ]

let doc_of_builtin b = (List.find (fun { name; _ } -> name = b) builtins).doc
