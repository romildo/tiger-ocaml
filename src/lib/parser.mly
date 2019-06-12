// parser.mly

%{
  open Absyn

  let at location x =
    let (left, right) = location in
    ({ Location.loc_start = left; Location.loc_end = right}, x)
%}

%token <int>    INT
%token <string> STR
%token <string> ID
%token          FOR WHILE BREAK LET IN NIL TO END
%token          FUNCTION VAR TYPE ARRAY IF THEN ELSE DO OF
%token          LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token          DOT COLON COMMA SEMI
%token          PLUS MINUS TIMES DIV UMINUS
%token          EQ NE LT LE GT GE
%token          AND OR
%token          ASSIGN
%token          EOF

%start <Absyn.lexp>    program

%%

program:
 | x=exp EOF { x }

exp:
 | NIL       { at $loc NilExp }
