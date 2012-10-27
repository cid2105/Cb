type token =
  | SHARP
  | FLAT
  | NCONST of (char)
  | LITERAL of (int)
  | COMMA
  | NOTE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | EOL

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
