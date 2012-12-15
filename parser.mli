type token =
  | INTLITERAL of (int)
  | DURATIONCONST of (string)
  | NOTECONST of (string)
  | BOOLLITERAL of (bool)
  | ID of (string)
  | LEFTPAREN
  | RIGHTPAREN
  | LBRAC
  | RBRAC
  | EOF
  | INT
  | NOTE
  | CHORD
  | SCALE
  | STANZA
  | SCORE
  | VOID
  | BOOL
  | IN
  | IF
  | ELSE
  | NOELSE
  | WHILE
  | FOREACH
  | ASSIGN
  | PLUSEQ
  | MINUSEQ
  | TIMESEQ
  | DIVIDEEQ
  | MOD
  | AND
  | OR
  | MODEQ
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | IS
  | ISNT
  | LT
  | LEQ
  | GT
  | GEQ
  | PLUSPLUS
  | MINUSMINUS
  | RAISE
  | LOWER
  | METH
  | RETURN
  | END
  | SEMICOLON
  | COMMA
  | DOT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
