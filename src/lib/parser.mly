// parser.mly

%{
  open Absyn
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

%start <Absyn.exp>    program

%%

program:
 | x=exp EOF { x }

exp:
 | NIL       { NilExp }
