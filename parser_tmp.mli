type token =
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | LSBRACK
  | RSBRACK
  | SEMI
  | COMMA
  | DOT
  | P
  | M
  | TIMES
  | DIV
  | MOD
  | PP
  | MM
  | SHARP
  | FLAT
  | ASSIGN
  | PEQ
  | MEQ
  | TEQ
  | DIVEQ
  | MODEQ
  | LOWER
  | RAISE
  | IS
  | ISNT
  | LT
  | LQT
  | GT
  | GEQ
  | IF
  | ELSE
  | NOELSE
  | ELIF
  | FOREACH
  | IN
  | WHILE
  | RETURN
  | INT
  | NOTE
  | CHORD
  | SCORE
  | STANZA
  | SCALE
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | WHOLE
  | HALF
  | QUARTER
  | EIGHT
  | SIXTEENTH
  | LITERAL of (int)
  | ID of (string)
  | DATATYPE of (string)
  | LCOMM
  | RCOMM
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
