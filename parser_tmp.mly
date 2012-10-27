%{ open Ast %} 
%token LPAREN RPAREN LBRAC RBRAC LSBRACK RSBRACK 
%token SEMI COMMA DOT 
%token P M TIMES DIV MOD PP MM POUND
%token ASSIGN PEQ MEQ TEQ DIVEQ MODEQ LOWER RAISE /* = += -= *= /= %= */
%token IS ISNT LT LQT GT GEQ                     /* COMPARE > < >= <= "is" "isnt" */
%token IF ELSE ELIF FOR IN WHILE RETURN          /* foreach in,  RETURN is to be removed */  
%token INT BOOL NOTE CHORD SCORE STANZAS SCALE
%token A B C D E F G
%token <int> LITERAL
%token <string> ID
%token <string> DATATYPE
%token LCOMM
%token RCOMM
%token EOF 
%nonassoc ELSE  
%nonassoc ELIF 
%nonassoc LPAREN 
%left PEQ MEQ 
%left TIMESEQ DIVEQ MODEQ 
%right ASSIGN  
%left IS ISNT LT GT LEQ GEQ
%left P M 
%left TIMES DIV MOD
%left PP MM 
%start program 
%type <Ast.program> program 
%% 
program: 
/* nothing */ { [], [] } 
