%{
open Language

let range {start;_} {fin;_} = {start; fin}

let i (Just (_, i)) = i
let it (Ty (_, i)) = i

let updateRE start fin (Just(e, _)) = Just(e, range start fin)
let updateRT start fin (Ty(t, _)) = Ty(t, range start fin)
%}

%token <string * Language.pos_info> IDENT
%token <int * Language.pos_info>    NUM
%token <bool * Language.pos_info>   BOOL

%token <Language.pos_info> TNAT
%token <Language.pos_info> TBOOL
%token <Language.pos_info> LAM
%token <Language.pos_info> LPAREN
%token <Language.pos_info> RPAREN
%token <Language.pos_info> COLON
%token <Language.pos_info> DOT
%token <Language.pos_info> USCORE
%token <Language.pos_info> LET
%token <Language.pos_info> IN
%token <Language.pos_info> EQ
%token <Language.pos_info> ARROW
%token <Language.pos_info> QMARK
%token <Language.pos_info> IF
%token <Language.pos_info> THEN
%token <Language.pos_info> ELSE
%token <Language.pos_info> REF
%token <Language.pos_info> DEREF
%token <Language.pos_info> DEFEQ
%token <Language.pos_info> SEMI
%token EOF

%start toplevel_expr
%type <(unit -> triv_ty) -> unelaborated_expr> toplevel_expr
%%

toplevel_expr:
  | expr EOF { fun ft -> $1 ft }

expr:
  | app_like_expr { fun ft -> $1 ft }
  | LAM IDENT DOT expr { fun ft ->
      let body = $4 ft in
      Just(Lam($2, Language.ft TUnknown, body), range $1 (i body)) }
  | LAM IDENT COLON ty DOT expr { fun ft ->
      let ty, body = $4 ft, $6 ft in
      Just(Lam($2, ty, body), range $1 (i body)) }
  | LET IDENT EQ expr IN expr { fun ft ->
      let value, body = $4 ft, $6 ft in
      let fullrange = range $1 (i body) in
      Just(App(Just(Lam($2, Language.ft TUnknown, body), fauxinfo), value, `DesugaredLet),
           fullrange)
  }
  | LET IDENT COLON ty EQ expr IN expr { fun ft ->
      let ty, value, body = $4 ft, $6 ft, $8 ft in
      let fullrange = range $1 (i body) in
      Just(App(Just(Lam($2, ty, body), fauxinfo), value, `DesugaredLet), fullrange)
  }
  | IF expr THEN expr ELSE expr { fun ft ->
      let cond, thn, els = $2 ft, $4 ft, $6 ft in
      Just(If(cond, thn, els), range (i cond) (i els)) }
  | REF atomic_expr { fun ft ->
      let value = $2 ft in
      Just(Ref(value), range $1 (i value)) }
  | expr DEFEQ expr { fun ft ->
      let ref = $1 ft in
      let value = $3 ft in
      Just(RefAssign(ref, value), range (i ref) (i value)) }
  | expr DEFEQ expr SEMI expr { fun ft ->
      let ref = $1 ft in
      let value = $3 ft in
      let body = $5 ft in
      Just(
        App(Just(Lam(("_", fauxinfo), Language.ft TUnknown, body), i body),
            Just(RefAssign(ref, value), range (i ref) (i value)),
            `DesugaredSeq),
        range (i ref) (i body))
  }

app_like_expr:
  | atomic_expr { fun ft -> $1 ft }
  | app_like_expr atomic_expr { fun ft ->
      let left, right = $1 ft, $2 ft in
      Just(App(left, right, `App), range (i left) (i right)) }

atomic_expr:
  | LPAREN expr RPAREN { fun ft -> updateRE $1 $3 ($2 ft) }
  | IDENT { fun _ -> Just(Var (`Local (fst $1)), snd $1) }
  | NUM { fun _ -> Just(Nat (fst $1), snd $1) }
  | BOOL { fun _ -> Just(Bool (fst $1), snd $1) }
  | DEREF atomic_expr { fun ft ->
      let value = $2 ft in
      Just(Deref(value), range $1 (i value)) }

ty:
  | arrow_like_type { fun ft -> $1 ft }
  | REF atomic_type { fun ft ->
      let inner = $2 ft in
      Ty (TRef inner, range $1 (it inner)) }

arrow_like_type:
  | atomic_type { fun ft -> $1 ft }
  | atomic_type ARROW arrow_like_type { fun ft ->
      let left, right = $1 ft, $3 ft in
      Ty (TArrow(left, right), range (it left) (it right)) }

atomic_type:
  | LPAREN ty RPAREN { fun ft -> updateRT $1 $3 ($2 ft) }
  | TNAT   { fun _ -> Ty (TNat, $1) }
  | TBOOL  { fun _ -> Ty (TBool, $1) }
  | QMARK  { fun _ -> Ty (TUnknown, $1) }
  | USCORE { fun ft ->
      let Ty (ty, _) = fauxify (ft()) in
      Ty(ty, $1) }
