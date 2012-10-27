%{ open Ast_tmp %} 

%token LPAREN RPAREN LBRAC RBRAC LSBRACK RSBRACK 
%token SEMI COMMA DOT 
%token P M TIMES DIV MOD PP MM SHARP FLAT
%token ASSIGN PEQ MEQ TEQ DIVEQ MODEQ LOWER RAISE  /* = += -= *= /= %= ^- ^+ */
%token IS ISNT LT LQT GT GEQ                       /* COMPARE > < >= <= "is" "isnt" */
%token IF ELSE NOELSE ELIF FOREACH IN WHILE RETURN /* foreach in,  RETURN is to be removed */  
%token INT BOOL NOTE CHORD SCORE STANZAS SCALE
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
  td_type IDENT { ($1, $2) }

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

object: Note {} | Chord {} | Stanza {}

collection: Chord {} | Stanza {} | Scale {} | Score {}

stmt:
  expr SEMI { Expr($1) }
  | LBRAC stmt_list RBRAC { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | FOREACH LPAREN object IN collection RPAREN stmt { Foreach($3, $5, $7) }
  /* NEW STATEMENTS*/
  | expr SEMICOLON
      { Execute($1) }
  | typed_id SEMICOLON
      { VarDecl($1) }
  | IF OPENPAREN expr CLOSEPAREN stmt ELSE stmt
      { IfThenElse($3, $5, $7) }
  | IF OPENPAREN expr CLOSEPAREN stmt %prec NOELSE
      { IfThenElse($3, $5, Block([])) }
  | FOR OPENPAREN expr SEMICOLON expr SEMICOLON expr CLOSEPAREN stmt
      { For($3, $5, $7, $9) }
  | FOREACH OPENPAREN typed_id IN expr CLOSEPAREN stmt
      { Foreach($3, $5, $7) }
  | WHILE OPENPAREN expr CLOSEPAREN stmt
      { While($3, $5) }
  | RETURN expr SEMICOLON
      { Return(Some($2)) }
  | RETURN SEMICOLON
      { Return(None) }
  | OPENBRAC stmts CLOSEBRAC
      { Block(List.rev $2) }


expr:
  LITERAL { Literal($1) }
  | ID { Id($1) }
  | expr P expr { Binop($1, Add, $3) }
  | expr M expr { Binop($1, Sub, $3) }
  | expr TIMES expr { Binop($1, Mult, $3) }
  | expr DIV expr { Binop($1, Div, $3) }
  | expr IS expr { Binop($1, Is, $3) }
  | expr ISNT expr { Binop($1, Isnt, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr LEQ expr { Binop($1, Leq, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr GEQ expr { Binop($1, Geq, $3) }
  | LOWER expr    { Lower($2) }
  | RAISE expr    { Raise($2) }
  | ID ASSIGN expr { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

expr_opt:
/* nothing */ { Noexpr }
| expr { $1 }

actuals_opt:
  /* nothing */ { [] }
  | actuals_list { List.rev $1 }

actuals_list:
  expr { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }