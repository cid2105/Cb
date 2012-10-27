%{ open Ast_tmp %} 

%token LPAREN RPAREN LBRAC RBRAC LSBRACK RSBRACK 
%token SEMI COMMA DOT 
%token P M TIMES DIV MOD PP MM SHARP FLAT
%token ASSIGN PEQ MEQ TEQ DIVEQ MODEQ LOWER RAISE  /* = += -= *= /= %= ^- ^+ */
%token IS ISNT LT LQT GT GEQ                       /* COMPARE > < >= <= "is" "isnt" */
%token IF ELSE NOELSE ELIF FOR FOREACH IN WHILE RETURN /* foreach in,  RETURN is to be removed */  
%token INT BOOL NOTE CHORD SCORE STANZA SCALE
%token A B C D E F G
%token WHOLE HALF QUARTER EIGHT SIXTEENTH 
%token <int> LITERAL
%token <string> ID
%token <string> DATATYPE
%token LCOMM
%token RCOMM
%token EOF 
%nonassoc NOELSE
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

td_type:
  | INT      { Int }
  | NOTE   { Note }
  | CHORD     { Chord }
  | SCORE    { Score }
  | STANZA { Stanza }
  | SCALE     { Scale }

typed_id:
  td_type ID { ($1, $2) }

program: 
  /* nothing */ { [], [] }
  | program vdecl { ($2 :: fst $1), snd $1 } 
  | program fdecl { fst $1, ($2 :: snd $1) } 

fdecl: 
  DATATYPE ID LPAREN formals_opt RPAREN LBRAC vdecl_list stmt_list RBRAC 
            { { fname = $2; 
                rettype = $1; 
                formals = $4; 
                locals = List.rev $7; 
                body = List.rev $8 } } 

formals_opt:
  /* nothing */ { [] }
  | formal_list { List.rev $1 }

formal_list:
  ID { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list: 
  /* nothing */    { [] } 
  | vdecl_list vdecl { $2 :: $1 }

vdecl: 
  DATATYPE ID SEMI { { varname = $2; vartype = $1; value = 0 } } 

stmt_list:
  /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

iterable_id:
  iterable_type ID { ($1, $2) }

iterable_type: NOTE { Note } | CHORD { Chord } | STANZA { Stanza }

stmt:
  expr SEMI { Expr($1) }
  | LBRAC stmt_list RBRAC { Block(List.rev $2) }
  /* NEW STATEMENTS*/
  | expr SEMI
      { Execute($1) }
  | typed_id SEMI
      { VarDecl($1) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
      { IfThenElse($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE
      { IfThenElse($3, $5, Block([])) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt
      { For($3, $5, $7, $9) }
  | FOREACH LPAREN iterable_id IN expr RPAREN stmt
      { Foreach($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt
      { While($3, $5) }
  | RETURN expr SEMI
      { Return(Some($2)) }
  | RETURN SEMI
      { Return(None) }
  | LBRAC stmt_list RBRAC
      { Block(List.rev $2) }


expr:
  ID { Id($1) }                                       /* x        */
  | Literal { Literal($1) }                     /* 1        */
  | expr P expr { Binop($1, Add, $3) }                /* x + y    */
  | expr M expr { Binop($1, Sub, $3) }                /* x - y    */
  | expr TIMES expr { Binop($1, Mult, $3) }           /* x * y    */
  | expr DIV expr { Binop($1, Div, $3) }              /* x / y    */
  | expr IS expr { Binop($1, Is, $3) }                /* x == y   */
  | expr ISNT expr { Binop($1, Isnt, $3) }            /* x != y   */
  | expr LT expr { Binop($1, Less, $3) }              /* x < y    */
  | expr LEQ expr { Binop($1, Leq, $3) }              /* x <= y   */
  | expr GT expr { Binop($1, Greater, $3) }           /* x > y    */
  | expr GEQ expr { Binop($1, Geq, $3) }              /* x >= y   */
  | LOWER expr    { Lower($2) }                       /* ^- x     */
  | RAISE expr    { Raise($2) }                       /* ^+ x     */
  | ID ASSIGN expr { Assign($1, $3) }                 /* x = y    */
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }     /* x(...)   */
  | LPAREN expr RPAREN { $2 }                         /* (x)      */
  /* WHICH A SHARP?? */
  | ID SHARP { $1 }                                   /* A#       */ 
  | ID FLAT { $1 }                                    /* A#       */ 

expr_opt:
/* nothing */ { Noexpr }
| expr { $1 }

actuals_opt:
  /* nothing */ { [] }
  | actuals_list { List.rev $1 }

actuals_list:
  expr { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }