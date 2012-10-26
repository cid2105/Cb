%{ open Ast %}

%token SEMI LPAREN REPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE FOR WHILE INT EOF
%token <int> LITERAL
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type < Ast.program> program

%%

program:
  /* nothing */ { [], [] }
| program vdecl {}
| program fedcl {}

expr:
  expr OR  expr       { Or($1, $3)  }
| expr AND expr       { And($1, $3) }
| product             { $1 }

product:
| product term        { And($1, $2) }
| term                { $1 }

term:
  NOT term         { Not($2) }
| ID               { Id($1)  }
| LPAREN expr RPAREN { $2 }
