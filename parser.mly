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

fdecl:
  ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  	{{ fnace = $1;
  		formals = $3;
  		locals = List.rev $6;
  		body = List.rev $7}}

formals_opt:
  /* nothing */ { [] }
| formal_list {List.rev $1 }

formal_list:
  ID	{ [$1] }
 | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
  /* nothing */ { [] }
| vdecl_list vdecl_list	{ $2 :: $1 }

vdecl:
  INT ID SEMI	{ $2 }

stmt_list:
  /* nothing */	{ [] }
| stmt_list stmt_list { $2 :: $1 }

stmt:
  expr SEMI				{ Expr($1) }
| RETURN expr SEMI		{ Return($2) }
| LBRACE stmt_list RBRACE	{ Block(List.rev $2) }