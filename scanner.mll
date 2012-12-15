{ open Parser } (* Get the token types *)

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
	| "<-" { comment lexbuf } (* Comments *)
	| '(' { LEFTPAREN }
	| ')' { RIGHTPAREN } (* punctuation *)
	| '[' { LBRAC } (* punctuation *)
	| ']' { RBRAC } (* punctuation *)
	| ';' { SEMICOLON }
	| ',' { COMMA }
	| '.' { DOT }
	| '+' { PLUS } (* started here *)
	| '-' { MINUS }
	| '*' { TIMES }
	| '/' { DIVIDE }
	| '%' { MOD }
	| '=' { ASSIGN }
	| "end" { END }
	| "+=" { PLUSEQ }
	| "-=" { MINUSEQ }
	| "*=" { TIMESEQ }
	| "/=" { DIVIDEEQ }
	| "%=" { MODEQ }
	| "++" { PLUSPLUS }
	| "--" { MINUSMINUS }
	| "^+" { RAISE }
	| "^-" { LOWER }
	| '<' { LT }
	| "<=" { LEQ }
	| ">" { GT }
	| ">=" { GEQ }
	| "and" { AND }
	| "or" { OR }
	| "if" { IF } (* keywords *)
	| "else" { ELSE }
	(*| "elseif" { ELSIF } *)
	| "foreach" { FOREACH }
	| "in" { IN }
	| "is" { IS }
	| "isnt" { ISNT }
	| "while" { WHILE }
	| "return" { RETURN }
	(* | "void" | "int" | "bool" | "note" | "chord" | "scale" | "stanza" | "score" { SCORE } *)
	| "void" { VOID}
	| "int" { INT }
	| "bool" { BOOL }
	| "note" { NOTE }
	| "chord" { CHORD }
	| "scale" { SCALE }
	| "stanza" { STANZA }
	| "score" { SCORE }
	| "meth" { METH }
	| "return" { RETURN }
	| "end" { END }
	| "true"|"false" as boollit { BOOLLITERAL(bool_of_string boollit) }
	(*| '-'? ['0' - '5'] as octave { OCTAVE(int_of_string octave) }   *)        (*mn always between -5 and 5 *)
	(*| ['1'-'9'](['0'-'9']) as durInt { DURATIONINT(int_of_string durInt) }*)	(*mn only positive int *)
	| ((['A'-'G'](['b' '#']?))|'R') as noteconst { NOTECONST(noteconst) }
	| ("whole" | "half" | "quarter") as durConst { DURATIONCONST(durConst) }
	| eof { EOF } (* Endoffile *)
	| '-'?['0'-'9']['0'-'9']* as lxm { INTLITERAL(int_of_string lxm) } (* integers *)
	| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
	| _ as char { raise (Failure("illegal character: " ^ Char.escaped char)) }

	and comment = parse
	"->" { token lexbuf } (* Endofcomment *)
	| _ { comment lexbuf } (* Eat everything else *)
