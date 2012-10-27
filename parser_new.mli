type token =
  | INTLITERAL of (int)
  | STRING of (string)
  | DATATYPE of (string)
  | ID of (string)
  | ASSIGN
  | SEMICOLON

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast_tmp.program
