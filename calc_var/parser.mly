%{
  open Grammar
%}

%token <int> NUMERAL
%token <string> VARIABLE
%token EQUAL
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token UMINUS
%token LPAREN
%token RPAREN
%token EOF

/* associativity */
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start toplevel
%type <Grammar.statement> toplevel


%%

toplevel:
  | e = expression EOF
    { Expression e }
  | x = VARIABLE EQUAL e = expression EOF
    { Definition (x, e) }
;

expression:
  | x = VARIABLE { Variable x }
  | n = NUMERAL { Numeral n }
  | l = expression TIMES  r = expression { Times (l, r) }
  | l = expression DIVIDE r = expression { Divide (l, r) }
  | l = expression PLUS   r = expression { Plus (l, r) }
  | l = expression MINUS  r = expression { Minus (l, r) }
  | MINUS e = expression %prec UMINUS { Negate e }
  | LPAREN e = expression RPAREN { e }
;
