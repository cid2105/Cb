 (* File lexer.mll *)
{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}
rule token = parse
  [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | ','            { COMMA }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '#'            { SHARP }
  | 'b'            { FLAT }
  | "Note"         { NOTE }
  | ['A' - 'G'] as const { NCONST( const ) }
  | eof            { raise Eof }