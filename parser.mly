%{ open Ast %}

%token OR AND NOT LPAREN RPAREN EOF
%token <char> ID

%left OR
%left AND
%right NOT

%start expr
%type < Ast.expr> expr

%%

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
