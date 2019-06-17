// parser.mly

%{
  open Absyn
%}

%token <int>           INT
%token <string>        STR
%token <Symbol.symbol> ID
%token                 FOR WHILE BREAK LET IN NIL TO END
%token                 FUNCTION VAR TYPE ARRAY IF THEN ELSE DO OF
%token                 LPAREN "(" RPAREN ")"
%token                 LBRACK "[" RBRACK "]"
%token                 LBRACE "{" RBRACE "}"
%token                 DOT "." COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/"
%token                 UMINUS
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND OR
%token                 ASSIGN ":="
%token                 EOF

%start <Absyn.lexp>    program

%%

program:
 | x=exp EOF                   { x }

exp:
 | NIL                         {$loc, NilExp}
 | x=INT                       {$loc, IntExp x}
 | t=ID "[" i=exp "]" OF x=exp {$loc, ArrayExp (t, i, x)}
 | x=exp o=binop y=exp         {$loc, OpExp (o, x, y)}

%inline binop:
 | "+"                         {PlusOp}
