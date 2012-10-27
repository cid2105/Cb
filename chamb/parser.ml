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

open Parsing;;
# 1 "parser.mly"
 open Ast 
# 20 "parser.ml"
let yytransl_const = [|
  257 (* SHARP *);
  258 (* FLAT *);
  261 (* COMMA *);
  262 (* NOTE *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* LPAREN *);
  268 (* RPAREN *);
  269 (* EOL *);
    0|]

let yytransl_block = [|
  259 (* NCONST *);
  260 (* LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000\
\007\000\007\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\012\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\003\000\000\000\000\000\006\000\007\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\000\009\000\
\010\000"

let yydgoto = "\002\000\
\007\000\008\000"

let yysindex = "\011\000\
\007\255\000\000\000\000\003\255\007\255\007\255\000\000\012\255\
\013\255\000\000\026\255\007\255\007\255\007\255\007\255\000\000\
\001\255\000\000\251\254\251\254\000\000\000\000\025\255\032\255\
\035\255\036\255\037\255\005\255\030\255\031\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\255\019\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\251\255"

let yytablesize = 43
let yytable = "\010\000\
\011\000\023\000\024\000\014\000\015\000\025\000\019\000\020\000\
\021\000\022\000\003\000\001\000\004\000\009\000\005\000\017\000\
\031\000\006\000\012\000\013\000\014\000\015\000\004\000\004\000\
\016\000\005\000\005\000\004\000\004\000\026\000\005\000\005\000\
\012\000\013\000\014\000\015\000\027\000\018\000\028\000\029\000\
\030\000\032\000\033\000"

let yycheck = "\005\000\
\006\000\001\001\002\001\009\001\010\001\005\001\012\000\013\000\
\014\000\015\000\004\001\001\000\006\001\011\001\008\001\003\001\
\012\001\011\001\007\001\008\001\009\001\010\001\007\001\008\001\
\013\001\007\001\008\001\012\001\013\001\005\001\012\001\013\001\
\007\001\008\001\009\001\010\001\005\001\012\001\004\001\004\001\
\004\001\012\001\012\001"

let yynames_const = "\
  SHARP\000\
  FLAT\000\
  COMMA\000\
  NOTE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  NCONST\000\
  LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
           ( _1 )
# 118 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 28 "parser.mly"
                             ( Lit(_1) )
# 125 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
                            ( _2 )
# 132 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                             ( Binop(_1, Add, _3) )
# 140 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                             ( Binop(_1, Sub, _3) )
# 148 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                             ( Binop(_1, Times, _3) )
# 156 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                             ( Binop(_1, Div, _3) )
# 164 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                            ( Inv(_2) )
# 171 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : char) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 35 "parser.mly"
                                                    ( Note(_3, _6, 2) )
# 179 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : char) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 36 "parser.mly"
                                                  ( Note(_3, _6, 1) )
# 187 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : char) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 37 "parser.mly"
                                            ( Note(_3, _5, 0) )
# 195 "parser.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
