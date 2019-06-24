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
 | x=exp EOF                             {x}

exp:
 | NIL                                   {$loc, NilExp}
 | x=INT                                 {$loc, IntExp x}
 | t=ID "[" n=exp "]" OF x=exp           {$loc, ArrayExp (t, n, x)}
 | x=exp o=binop y=exp                   {$loc, OpExp (o, x, y)}
 | x=var                                 {$loc, VarExp x}
 | "(" es=exps ")"                       {$loc, SeqExp es}
 | LET ds=decs IN es=exps END            {$loc, LetExp (ds, ($loc(es), SeqExp es))}
 (* TODO: complete with remaining expressions *)

%inline binop:
 | "+"                                   {PlusOp}
 (* TODO: complete with remaining binary operators *)

exps:
 | es=separated_list(";", exp)           {es}

var:
 | v=ID                                  {$loc, SimpleVar v}
 (* TODO: complete with remaining variables *)

decs:
 | ds=list(dec)                          {ds}

dec:
 | d=vardec                              {$loc, d}
 | d=mutualtypedecs                      {$loc, d}
 (* TODO: complete with remaining declarations *)

vardec:
 | VAR v=ID t=type_constraint ":=" e=exp {VarDec (v, t, e)}

typedec:
 | TYPE t=ID "=" ty=ty                   {$loc, (t, ty)}

mutualtypedecs:
 | ds=nonempty_list(typedec)             {MutualTypeDecs ds}

type_constraint:
 | c=option(":" t=ID                     {$loc(t), t}) {c}

ty:
 | ty=ID                                 {$loc, NameTy ty}
 (* TODO: complete with remaining type constructors *)
