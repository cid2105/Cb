{ open Parser } (* Get the token types *)

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
	| "<-" { comment lexbuf } (* Comments *)
	| '(' { LPAREN } 
	| ')' { RPAREN } (* punctuation *)
	| ';' { SEMI } 
	| ',' { COMMA }
	| '.' { DOT }
	| '+' { PLUS } (* started here *)
	| '-' { MINUS }
	| '*' { TIMES } 
	| '/' { DIVIDE }
	| '%' { MOD }
	| "end" { END }
	| "+=" { PLUSEQ }
	| "-=" { MINUSEQ }
	| "*=" { TIMESEQ }
	| "/=" { DIVIDEEQ }
	| "%=" { MODEQ }
	| '=' { ASSIGN } 
	| '!' { NOT }
	| "++" { PLUSPLUS }
	| "--" { MINUSMINUS }
	| "==" { EQ }
	| "!=" { NEQ } 
	| '<' { LT }
	| "<=" { LEQ } 
	| ">" { GT }
	| ">=" { GEQ } 
	| "&&" { AND }
	| "||" { OR }
	| "if" { IF } (* keywords *) 
	| "else" { ELSE } 
	| "foreach" { FOR }
	| "in" { IN }
	| "while" { WHILE } 
	| "return" { RETURN }
	| "void" { DATATYPE("void") }
	| "int" { DATATYPE("int") }
	| "bool" { DATATYPE("bool") }
	| "note" { DATATYPE("note") }
	| "chord" { DATATYPE("chord") }
	| "scale" { DATATYPE("scale") }
	| "stanza" { DATATYPE("stanza") }
	| "score" { DATATYPE("score") }
	| "true"|"false" as boollit { BOOLLITERAL(bool_of_string boollit) }
	| (['a'-'g' 'A'-'G']['s' 'f' 'S' 'F']?['0'-'9'])|('r'|'R') as pitchlit { PITCHLITERAL(pitchlit) }
	| eof { EOF } (* Endoffile *)
	| ['0'-'9']+ as lxm { INTLITERAL(int_of_string lxm) } (* integers *)
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
	| _ as char { raise (Failure("illegal character: " ^ Char.escaped char)) }

	and comment = parse
	"->" { token lexbuf } (* Endofcomment *)
	| _ { comment lexbuf } (* Eat everything else *)