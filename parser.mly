%{ open Ast %}

%token <int> INTLITERAL
%token <int> OCTAVE /* integer between -5 and 5 */
%token <int> DURATIONINT /* positive intege x>0 */

%token <string> DURATIONCONST /* whole half etc. */
%token <string> NOTECONST  /* Goes to string A or B or any note*/
%token <bool> BOOLLITERAL
%token <string> ID

%token LEFTPAREN RIGHTPAREN LBRAC RBRAC EOF
%token INT NOTE CHORD SCALE STANZA SCORE VOID

%token BOOL /* Added bool */
%token IN
%token IF
%token ELSE NOELSE ELSIF
%token WHILE FOREACH
%token ASSIGN
%token PLUSEQ
%token MINUSEQ
%token TIMESEQ
%token DIVIDEEQ
%token MOD
%token AND
%token OR
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
%left OR
%left AND   /* ex: a > b && c is 3 or isnt A  => ((a > b) && (c is 3)) or (isnt A) */
%left IS ISNT
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left PLUSPLUS MINUSMINUS RAISE LOWER
%left SHARP FLAT

%start program
%type <Ast.program> program		/* ocamlyacc: e - no type has been declared for the start symbol `program'*/

%%

program:
	methdecl { $1 }

vdecl:
	cb_type ID SEMICOLON
		{{ vartype = $1;
			varname = $2 }}

vdecl_list:
	/* nothing */    { [] }
	| vdecl_list vdecl { $2 :: $1 }

methdecl:
	METH cb_type ID LEFTPAREN meth_params RIGHTPAREN vdecl_list statement_list END /* m stuff */
		{ {
			rettype = $2;
			fname = $3;
			formals = $5;
			locals = List.rev $7;
			body = List.rev $8 } }

cb_type:
      INT                                 	{ Int }
    | NOTE                               	{ Note }
    | CHORD                                 { Chord }
    | SCALE                              	{ Scale }
    | BOOL                               	{ Bool }
    | STANZA                             	{ Stanza }
    | SCORE                             	{ Score }
	| VOID                               	{ Void }

meth_params:
	{ [] }
	| param_list { List.rev($1) }

param_list:
	param_decl { [$1] }
	| param_list COMMA param_decl { $3 :: $1 }

param_decl:
	cb_type ID
		{ {	paramname = $2;
			paramtype = $1 } }

statement_list:
	{ [] }
	| statement_list statement { $2 :: $1 }

statement:
	expr SEMICOLON { Expr($1) }
	| RETURN expr_opt SEMICOLON { Return($2) }   /*mn possibly nothing to return*/
	| RIGHTPAREN statement_list END { Block(List.rev $2) }
	| IF LEFTPAREN expr RIGHTPAREN statement_list elsif_statement %prec NOELSE END { If($3, $5, $6, Block([])) }
	| IF LEFTPAREN expr RIGHTPAREN statement_list elsif_statement ELSE statement_list END { If($3, $5, $6, Block(List.rev $8)) }
	| WHILE LEFTPAREN expr RIGHTPAREN statement_list END { While($3, $5) }
	| FOREACH LEFTPAREN param_decl IN ID RIGHTPAREN statement_list END { Foreach($3, $5, $7)}

elsif_statement:
      /* nothing */ { Block([]) }
	| ELSIF LEFTPAREN expr RIGHTPAREN statement_list { ElseIf($3, $5) }

duration_expr:
	DURATIONINT { DurInt($1) }
	| DURATIONCONST { DurConst($1) }								/* 5 			*/
	/* | duration_expr PLUS duration_expr { Binop($1, Add, $3)  }
	| duration_expr MINUS duration_expr { Binop($1, Sub, $3)  }
	| duration_expr TIMES duration_expr { Binop($1, Mult, $3)  }
	| duration_expr DIVIDE duration_expr { Binop($1, Div, $3)  }*/

generic_list:
	 /*mn cannot have empty ???*/ { [] }
	| generic_list COMMA expr {$3 :: $1}/* Depends on the type of id */
	/*mn | generic_list COMMA ID TIMES INTLITERAL { BinOp(Id($1), IDTimes, IntLiteral($3))} confusing  a, b, c*5, b  */

expr_opt:
	/* nothing */ { NoExpr }
	| expr { $1 }														/*  */

expr:
	ID { Id($1) }
	| ID DOT ID { MemberAccess($1, $3) }								/* score.put 	*/
	| INTLITERAL { IntLiteral($1) }
	| NOTECONST { NoteConst($1)}
	| BOOLLITERAL {BoolLiteral($1)}
	/*| ID LBRAC expr RBRAC { ElemOp($1, $3) }*/
	/*| DATATYPE ID ASSIGN expr { TypeAssign($1, $2, $4) }				 causing shift/reduce */
	| duration_expr { $1 }
	| LEFTPAREN NOTECONST COMMA OCTAVE COMMA duration_expr RIGHTPAREN  { NoteExpr($2, $4, $6) }  /* x = (A#, 4, 34) 		*/
	| LEFTPAREN LBRAC generic_list RBRAC COMMA duration_expr RIGHTPAREN   { ChordExpr($3, $6) }
	| LBRAC generic_list RBRAC { ListExpr($2) }
	/* | generic_list COMMA ID TIMES INTLITERAL { BinOp(Id($1), IDTimes, IntLiteral($3))}   a, b, c*5, b  */
    | expr ASSIGN expr 		{ Assign($1, $3) }									/* x = y 		*/
    | expr PLUSEQ expr { Assign($1, BinOp($1, Add, $3)) }				/* x += y		*/
	| expr MINUSEQ expr { Assign($1, BinOp($1, Sub, $3)) }				/* x -= y		*/
	| expr TIMESEQ expr { Assign($1, BinOp($1, Mult, $3)) }				/* x *= y		*/
	| expr DIVIDEEQ expr { Assign($1, BinOp($1, Div, $3)) }				/* x /=	y 		*/
	| expr MODEQ expr { Assign($1, BinOp($1, Mod, $3)) }				/* x %= y		*/
	| expr PLUS expr { BinOp($1, Add, $3) }								/* x + y		*/
	| expr MINUS expr { BinOp($1, Sub, $3) }							/* x - y		*/
	| expr TIMES expr { BinOp($1, Mult, $3) }							/* x * y		*/
	| expr DIVIDE expr { BinOp($1, Div, $3) }							/* x / y		*/
	| expr MOD expr { BinOp($1, Mod, $3) }
	| expr AND expr { BinOp($1, And, $3) }								/*mn (a is 4 and b is 2) */							/*x && y */
	| expr OR expr { BinOp($1, Or, $3) }								/*mn (a is 4 and b is 2) */							/* x % y		*/
	| expr IS expr { BinOp($1, Eq, $3) }								/* x is y		*/
	| expr ISNT expr { BinOp($1, NEq, $3) }								/* x isnt y		*/
	| expr LT expr { BinOp($1, Less, $3) }								/* x < y		*/
	| expr LEQ expr { BinOp($1, LEq, $3) }								/* x <= y		*/
	| expr GT expr { BinOp($1, Greater, $3) }							/* x > y		*/
	| expr GEQ expr { BinOp($1, GEq, $3) }								/* x >= y		*//* !x	        */
	| expr PLUSPLUS { Assign($1, BinOp($1, Add, IntLiteral(1))) }		/* x++			*/
	| expr MINUSMINUS { Assign($1, BinOp($1, Sub, IntLiteral(1))) }		/* x--			*/
	| expr SHARP { UnaryOp(Sharp, $1) }									/* A#			*/
	| expr FLAT  { UnaryOp(Flat, $1) }									/* Bb			*/
	| expr RAISE { UnaryOp(Raise, $1) }									/* x^-			*/
	| expr LOWER { UnaryOp(Lower, $1) }									/* x^+			*/
	| LEFTPAREN expr RIGHTPAREN { $2 }									/* (x)			*/
	| ID LEFTPAREN actuals_opt RIGHTPAREN { MethodCall($1, $3) }	/* x(...)		*/

actuals_opt:
	{ [] }
	| actuals_list { List.rev $1 }

actuals_list:
	expr { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }