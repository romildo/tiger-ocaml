%token X COMMA

%start <unit> s

%%

s: list(X; COMMA) {}
