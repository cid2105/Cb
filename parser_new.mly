%{ open Ast_tmp %} 

%token <int> INTLITERAL
%token <int> OCTAVE /* integer between -5 and 5 */
%token <int> DURATIONINT /* positive intege x>0 */

%token <string> DURATIONCONST /* whole half etc. */
%token <string> STRING
%token <string> DATATYPE
%token <string> NOTECONST  /* Goes to string A or B or any note*/
%token <string> ID

%token LEFTPAREN RIGHTPAREN LBRAC RBRAC
%token INT NOTE CHORD SCALE STANZA SCORE

%token METH RETURN END
%token PLUS MINUS TIMES DIV

%token ASSIGN
%token VASSIGN 	/* Variable Assign  only used for variable decleration */
%token SEMICOLON
%token COMMA

%left PLUS MINUS
%left TIMES DIV

%start program 
%type <Ast_tmp.program> program		/* ocamlyacc: e - no type has been declared for the start symbol `program'*/

%%

program:
{ [], [] }  									/* nothing 					*/
| program vdecl { ($2 :: fst $1), snd $1 }		/* variable declerations 	*/
| program methdecl { TODO() }					/* function declerations	*/

methdecl:
	METH DATATYPE ID LEFTPAREN meth_params RIGHTPAREN statement_list END { create() }

meth_params:
	{ [] }
	| param_list { List.rev($1) }

param_list:
	param_decl { [$1] }
	| param_list COMMA param_decl { $3 :: $1 }

param_decl:
	DATATYPE ID
		{ {	create{} } }

statement_list:
	ID { create{} }

vdecl: 
	DATATYPE ID SEMICOLON {{ vartype = $1; varname = $2}}
	| NOTE ID VASSIGN LEFTPAREN NOTECONST COMMA OCTAVE COMMA duration_expr RIGHTPAREN SEMICOLON { create() }
	| INT ID VASSIGN INTLITERAL SEMICOLON { create($2) }/* int x = 5; */
	| CHORD ID VASSIGN LEFTPAREN LBRAC generic_list RBRAC COMMA duration_expr RIGHTPAREN SEMICOLON  { create() }
	| SCALE ID VASSIGN LBRAC generic_list RBRAC { create{} }
	| STANZA ID VASSIGN LBRAC generic_list RBRAC { create{} } /* Add fancy shit about multiplying notes and chords dawg */
	| SCORE ID VASSIGN LBRAC generic_list RBRAC { create{} } /* Implement list of stanzas*/

generic_list:
	{ [%1] } /* cannot have empty */
	| generic_list COMMA ID { $3 :: $1 } /* Depends on the type of id */

duration_expr:
	DURATIONINT { $1 }
	| DURATIONCONST { $1 }
	| duration_expr PLUS duration_expr { Binop($1, Add, $3)  } 
	| duration_expr MINUS duration_expr { Binop($1, Sub, $3)  }
	| duration_expr TIMES duration_expr { Binop($1, Mult, $3)  }
	| duration_expr DIV duration_expr { Binop($1, Div, $3)  }


















