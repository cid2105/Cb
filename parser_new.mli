type token =
  | INTLITERAL of (int)
  | OCTAVE of (int)
  | DURATIONINT of (int)
  | DURATIONCONST of (string)
  | STRING of (string)
  | DATATYPE of (string)
  | NOTECONST of (string)
  | ID of (string)
  | LEFTPAREN
  | RIGHTPAREN
  | LBRAC
  | RBRAC
  | INT
  | NOTE
  | CHORD
  | SCALE
  | STANZA
  | SCORE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | ASSIGN
  | VASSIGN
  | SEMICOLON
  | COMMA

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast_tmp.program
