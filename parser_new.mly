%{ open Ast_tmp %} 

%token <int> INTLITERAL
%token <string> STRING
%token <string> DATATYPE
%token <string> ID
%token ASSIGN
%token SEMICOLON

%start program 
%type <Ast_tmp.program> program		/* ocamlyacc: e - no type has been declared for the start symbol `program'*/

%%

program:
{ [], [] }  									/* nothing 					*/
| program vdecl { ($2 :: fst $1), snd $1 }		/* variable declerations 	*/

vdecl: DATATYPE ID SEMICOLON {{ vartype = $1; varname = $2}}