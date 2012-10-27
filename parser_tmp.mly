%{ open Ast %} 
%token LPAREN RPAREN LBRAC RBRAC LSBRACK RSBRACK 
%token SEMI COMMA DOT 
%token P M TIMES DIV MOD PP MM POUND
%token ASSIGN PQ MEQ TEQ DIVEQ MODEQ LOWER RAISE /* = += -= *= /= %= */
%token IS ISNT LT LQT GT GEQ AND OR              /* COMPARE > < >= <= "is" "isnt" */
%token IF ELSE ELIF FOR IN WHILE RETURN          /* RETURN is to be removed */  
%token INT BOOL NOTE CHORD SCORE STANZAS SCALE
%token A B C D E F G
%token <int> LITERAL
%token <string> ID
%token <string> DATATYPE
%token EOF 
%nonassoc ELSE  
%nonassoc ELIF 
%nonassoc LPAREN 
%left PLUSEQ MINUSEQ 
%left TIMESEQ DIVIDEEQ MODEQ 
%right ASSIGN 
%left OR 
%left AND  
%left IS ISNT 
%left LT GT LEQ GEQ
%left P M 
%left TIMES DIV MOD 
%right UPLUS UMINUS 
%left PP MM 
%start program 
%type <Ast.program> program 
%% 
program: 
/* nothing */ { [], [] } 
