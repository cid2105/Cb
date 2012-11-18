%{ open Ast %} 

%token <int> INTLITERAL
%token <int> OCTAVE /* integer between -5 and 5 */
%token <int> DURATIONINT /* positive intege x>0 */

%token <string> DURATIONCONST /* whole half etc. */
%token <string> DATATYPE
%token <string> NOTECONST  /* Goes to string A or B or any note*/
%token <string> ID

%token LEFTPAREN RIGHTPAREN LBRAC RBRAC
%token INT NOTE CHORD SCALE STANZA SCORE

%token BOOL /* Added bool */
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

%token METH RETURN END
%token PLUS MINUS TIMES DIVIDE

%token ASSIGN 	/* Variable Assign  only used for variable decleration */
%token SEMICOLON
%token COMMA DOT

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELSIF
%left PLUSEQ MINUSEQ
%left TIMESEQ DIVIDEEQ MODEQ
%right ASSIGN
%left IS ISNT
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
| program methdecl { fst $1, ($2 :: snd $1) }	/* function declerations (m stuff)	*/

methdecl:
	METH DATATYPE ID LEFTPAREN meth_params RIGHTPAREN statement_list END /* m stuff */
		{ {	fname = $3;
			returntype = $2;
			formals = $5;
			body = List.rev $7 } }

meth_params:
	{ [] }
	| param_list { List.rev($1) }

param_list:
	param_decl { [$1] }
	| param_list COMMA param_decl { $3 :: $1 }

param_decl:
	DATATYPE ID 
		{ {	paramname = $2;
			paramtype = $1 } }

statement_list:
	{ [] }
	| statement_list statement { $2 :: $1 }

statement:
	expr SEMICOLON { Expr($1) }
	| RETURN expr SEMICOLON { Return($2) }
	| IF LEFTPAREN expr RIGHTPAREN statement_list elsif_statement %prec NOELSE END { If($3, $5, $6, Block([])) }
	| IF LEFTPAREN expr RIGHTPAREN statement_list elsif_statement ELSE statement_list END { If($3, $5, $6, $8) }
	| WHILE LEFTPAREN expr RIGHTPAREN statement_list END { While($3, Id($5)) }
	| FOREACH LEFTPAREN param_decl IN ID RIGHTPAREN statement_list END { Foreach($3, $5, $7)}

elsif_statement:
      /* nothing */ { [] }
	| elsif_statement ELSIF LEFTPAREN expr RIGHTPAREN statement_list { ElseIf($1, $4, $6) }

vdecl: 
	DATATYPE ID SEMICOLON 
		{{ vartype = $1; 
			varname = $2 }}
	
generic_list:
	{ [%1] } /* cannot have empty */
	| generic_list COMMA ID { $3 :: $1 } /* Depends on the type of id */
	| generic_list COMMA ID TIMES INTLITERAL { Binop($3, IDTimes, $5) :: $1 } /*  a, b, c*5, b  */

duration_expr:
	DURATIONINT { $1 }
	| DURATIONCONST { $1 }
	| duration_expr PLUS duration_expr { Binop($1, Add, $3)  }
	| duration_expr MINUS duration_expr { Binop($1, Sub, $3)  }
	| duration_expr TIMES duration_expr { Binop($1, Mult, $3)  }
	| duration_expr DIVIDE duration_expr { Binop($1, Div, $3)  }

expr:
	ID { Id($1) }														/* x 			*/
	| ID DOT ID { MemberAccess($1, $3) }								/* score.put 	*/
	| INTLITERAL { IntLiteral($1) }										/* 5 			*/
	| ID LBRAC expr RBRAC { ElemOp($1, $3) }
	/*| ID LBRAC expr RBRAC ASSIGN expr { LElemOp($1, $3, $6) }*/
	| ID ASSIGN expr { Assign($1, $3) }								/* x = y 		*/
	| DATATYPE ID ASSIGN expr { TypeAssign($1, $2, $3) }
	| ID ASSIGN LEFTPAREN NOTECONST COMMA OCTAVE COMMA duration_expr RIGHTPAREN  { NoteAssign($1, $4, $6, $8) }  /* x = (A#, 4, 34) 		*/
	| ID ASSIGN LEFTPAREN LBRAC generic_list RBRAC COMMA duration_expr RIGHTPAREN   { ChordAssign($1, $5, $8) }	
	| ID ASSIGN LBRAC generic_list RBRAC { ListAssign($1, $4) }	
	| DATATYPE ID ASSIGN LEFTPAREN NOTECONST COMMA OCTAVE COMMA duration_expr RIGHTPAREN  { TypeNoteAssign($1, $2, $5, $7, $9) }
	| DATATYPE ID ASSIGN LEFTPAREN LBRAC generic_list RBRAC COMMA duration_expr RIGHTPAREN   { TypeChordAssign($1, $2, $6, $9) }	
	| DATATYPE ID ASSIGN LBRAC generic_list RBRAC { TypeListAssign($1, $2, $5) }
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
	| expr GEQ expr { BinOp($1, GEq, $3) }								/* x >= y		*//* !x	        */
	| expr PLUSPLUS { Assign($1, BinOp($1, Add, IntLiteral(1))) }		/* x++			*/
	| expr MINUSMINUS { Assign($1, BinOp($1, Sub, IntLiteral(1))) }		/* x--			*/
	| expr SHARP { UnaryOp($1, Sharp) }									/* A#			*/
	| expr FLAT { UnaryOp($1, Flat) }									/* Bb			*/
	| expr RAISE { UnaryOp($1, Raise) }									/* x^-			*/
	| expr LOWER { UnaryOp($1, Lower) }									/* x^+			*/
	| LEFTPAREN expr RIGHTPAREN { $2 }									/* (x)			*/
	| ID LEFTPAREN actuals_opt RIGHTPAREN { MethodCall($1, $3) }	/* x(...)		*/

actuals_opt:
	{ [] }
	| actuals_list { List.rev $1 }

actuals_list:
	expr { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
