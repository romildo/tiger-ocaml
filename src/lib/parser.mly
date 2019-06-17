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

%nonassoc OF
%left PLUS

%start <Absyn.lexp>    program

%%

program:
 | x=exp EOF                    {x}

exp:
 | NIL                          {$loc, NilExp}
 | x=INT                        {$loc, IntExp x}
 | t=ID "[" n=exp "]" OF x=exp  {$loc, ArrayExp (t, n, x)}
 | x=exp o=binop y=exp          {$loc, OpExp (o, x, y)}
 | x=var                        {$loc, VarExp x}
 | LET ds=decs IN es=exps END   {$loc, LetExp (ds, ($loc(es), SeqExp es))}

%inline binop:
 | "+"                          {PlusOp}

var:
 | v=ID                         {$loc, SimpleVar v}

decs:
 | ds=list(dec)                 {ds}

dec:
 | VAR v=ID ":" t=ID ":=" e=exp {$loc, VarDec (v, Some ($loc(t), t), e)}

exps:
 | es=separated_list(";", exp)  {es}
