%{ open Ast %} 

%token <int> INTLITERAL
%token <int> OCTAVE /* integer between -5 and 5 */
%token <int> DURATIONINT /* positive intege x>0 */

%token <string> DURATIONCONST /* whole half etc. */
%token <string> STRING
%token <string> DATATYPE
%token <string> NOTECONST  /* Goes to string A or B or any note*/
%token <string> ID

%token IN
%token IF
%token ELSE NOELSE
%token WHILE FOREACH
%token ASSIGN
%token PLUSEQ
%token MINUSEQ
%token TIMESEQ
%token DIVIDEEQ
%token MOD
%token MODEQ
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token IS
%token ISNT
%token LT
%token LEQ
%token GT
%token GEQ
%token PLUSPLUS
%token MINUSMINUS
%token SHARP
%token FLAT
%token RAISE
%token LOWER

%token LEFTPAREN RIGHTPAREN LBRAC RBRAC
%token INT NOTE CHORD SCALE STANZA SCORE

%token METH RETURN END
%token PLUS MINUS TIMES DIVIDE

%token ASSIGN
%token VASSIGN 	/* Variable Assign  only used for variable decleration */
%token SEMICOLON
%token COMMA DOT

%nonassoc NOELSE
%nonassoc ELSE
%left PLUSEQ MINUSEQ
%left TIMESEQ DIVIDEEQ MODEQ
%right ASSIGN
%left IS ISNT AND OR
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left PLUSPLUS MINUSMINUS RAISE LOWER
%left SHARP FLAT

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
	DATATYPE ID { TODO() }

statement_list:
	{ [] }
	| statement_list statement { $2 :: $1 }

statement:
	expr SEMICOLON { TODO() }
	| RETURN expr_opt SEMICOLON { Return($2) }
	| IF LEFTPAREN expr RIGHTPAREN statement %prec NOELSE END { If($3, $5, Block([])) }
	| IF LEFTPAREN expr RIGHTPAREN statement ELSE statement END { If($3, $5, $7) }
	| WHILE LEFTPAREN expr RIGHTPAREN statement END { While($3, $5) }
	| FOREACH LEFTPAREN param_decl IN ID RIGHTPAREN statement END {TODO()}

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
	| duration_expr DIVIDE duration_expr { Binop($1, Div, $3)  }

expr_opt:
	{ NoExpr }
	| expr { $1 }	

expr:
	ID { Id($1) }														/* x 			*/
	| ID DOT ID { TODO() }												/* score.put 	*/
	| INTLITERAL { TODO() }												/* 5 			*/
	| expr ASSIGN expr { TODO() }										/* x = y 		*/
	| expr PLUSEQ expr { Assign($1, BinOp($1, Add, $3)) }				/* x += y		*/
	| expr MINUSEQ expr { Assign($1, BinOp($1, Sub, $3)) }				/* x -= y		*/
	| expr TIMESEQ expr { Assign($1, BinOp($1, Mult, $3)) }				/* x *= y		*/
	| expr DIVIDEEQ expr { Assign($1, BinOp($1, Div, $3)) }				/* x /=	y 		*/
	| expr MODEQ expr { Assign($1, BinOp($1, Mod, $3)) }				/* x %= y		*/
	| expr PLUS expr { BinOp($1, Add, $3) }								/* x + y		*/
	| expr MINUS expr { BinOp($1, Sub, $3) }							/* x - y		*/
	| expr TIMES expr { BinOp($1, Mult, $3) }							/* x * y		*/
	| expr DIVIDE expr { BinOp($1, Div, $3) }							/* x / y		*/
	| expr MOD expr { BinOp($1, Mod, $3) }								/* x % y		*/
	| expr IS expr { BinOp($1, Eq, $3) }								/* x is y		*/
	| expr ISNT expr { BinOp($1, NEq, $3) }								/* x isnt y		*/
	| expr LT expr { BinOp($1, Less, $3) }								/* x < y		*/
	| expr LEQ expr { BinOp($1, LEq, $3) }								/* x <= y		*/
	| expr GT expr { BinOp($1, Greater, $3) }							/* x > y		*/
	| expr GEQ expr { BinOp($1, GEq, $3) }								/* x >= y		*/										/* !x	        */  	                            					 
	| expr PLUSPLUS { Assign($1, BinOp($1, Add, IntLiteral(1))) }		/* x++			*/
	| expr MINUSMINUS { Assign($1, BinOp($1, Sub, IntLiteral(1))) }		/* x--			*/
	| expr SHARP { TODO() }												/* A#			*/
	| expr FLAT { TODO() }												/* Bb			*/
	| expr RAISE { TODO() }												/* x^-			*/
	| expr LOWER { TODO() }												/* x^+			*/
	| LEFTPAREN expr RIGHTPAREN { $2 }									/* (x)			*/ 
	| ID LEFTPAREN actuals_opt RIGHTPAREN { TODO() }					/* x(...)		*/
	| ID LBRAC expr RBRAC { ChordOp($1, $3) }						/* x[i]			*/


actuals_opt:
	{ [] }
	| actuals_list { List.rev $1 }

actuals_list:
	expr { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }