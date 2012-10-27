%{ open Ast %}

%token SHARP FLAT

%token <char> NCONST   /* basic note constants*/

%token <int> LITERAL
%token COMMA
%token NOTE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

                         /*  %start main           */  
                         /* %type <Ast.expr> main */
%start program            /* the entry point */
%type <Ast.expr> program

%%

program:
  expr EOL { $1 }
;
expr:
    LITERAL                  { Lit($1) }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr           { Binop($1, Add, $3) }
  | expr MINUS expr          { Binop($1, Sub, $3) }
  | expr TIMES expr          { Binop($1, Times, $3) }
  | expr DIV expr            { Binop($1, Div, $3) }
  | MINUS expr %prec UMINUS { Inv($2) }
  | NOTE LPAREN NCONST SHARP  COMMA LITERAL RPAREN  { Note($3, $6, 2) }
  | NOTE LPAREN NCONST FLAT  COMMA LITERAL RPAREN { Note($3, $6, 1) }
  | NOTE LPAREN NCONST COMMA LITERAL RPAREN { Note($3, $5, 0) }
  
  
;