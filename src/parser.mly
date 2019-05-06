// parser.mly

%token <int> INT
%token       EOF

%start program

%type <unit> program

%%

program:
| EOF { }
;

%%
