%{
  open Language
%}

%token <string> IDENT

%token TOP
%token BOT
%token LPAREN
%token RPAREN
%token AND
%token OR
%token ARROW
%token QUERY
%token EOF

%left ARROW
%left OR
%left AND

%start queries
%type <(ty * ty) list> queries
%%

queries:
  | EOF { [] }
  | query queries { $1::$2 }

query:
  | ty QUERY ty { ($1, $3) }

ty:
  | IDENT        { TPrim $1 }
  | TOP          { TTop }
  | BOT          { TBot }
  | ty ARROW ty  { TArrow($1, $3) }
  | ty OR ty     { TOp(Sup, $1, $3) }
  | ty AND ty    { TOp(Sub, $1, $3) }
  | LPAREN ty RPAREN { $2 }
