// parser.mly

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

%right    OF
%right    DO THEN ELSE
%nonassoc ASSIGN
%left     OR
%left     AND
%nonassoc EQ NE LT LE GT GE
%left     PLUS MINUS
%left     TIMES DIV
%right    UMINUS

%start <unit> program

%%

program:
 | exp EOF {}

var:
 | ID {}
 | ID LBRACK exp RBRACK {}
 | var LBRACK exp RBRACK {}
 | var DOT ID {}

dec:
 | VAR ID option(COLON ID {}) EQ exp {}
 | FUNCTION ID delimited(LPAREN, separated_nonempty_list(COMMA, param), RPAREN) option(COLON ID {}) EQ exp {}
 | TYPE ID EQ ty {}

ty:
 | ID {}
 | ARRAY OF ID {}
 | delimited(LBRACE, separated_nonempty_list(COMMA, param), RBRACE) {}

param:
 | separated_pair(ID, COLON, ID) {}

exp:
 | NIL {}
 | INT {}
 | STR {}
 | ID LBRACK exp RBRACK OF exp {}
 | delimited(LBRACE, separated_nonempty_list(COMMA, separated_pair(ID, EQ, exp)), RBRACE) {}
 | var {}
 | var ASSIGN exp {}
 | exp binop exp {}
 | MINUS exp {} %prec UMINUS
 | ID delimited(LPAREN, separated_nonempty_list(COMMA, exp), RPAREN) {}
 | IF exp THEN exp {}
 | IF exp THEN exp ELSE exp {}
 | WHILE exp DO exp {}
 | FOR ID ASSIGN EQ exp TO exp DO exp {}
 | BREAK {}
 | LET nonempty_list(dec) IN separated_nonempty_list(SEMI, exp) END {}
 | delimited(LPAREN, exp, RPAREN) {}

%inline binop:
  | PLUS {}
  | MINUS {}
  | TIMES {}
  | DIV {}
  | EQ {}
  | NE {}
  | LT {}
  | LE {}
  | GT {}
  | GE {}
  | AND {}
  | OR {}
