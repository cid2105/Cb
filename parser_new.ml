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

open Parsing;;
let _ = parse_error;;
# 1 "parser_new.mly"
 open Ast_tmp 
# 34 "parser_new.ml"
let yytransl_const = [|
  265 (* LEFTPAREN *);
  266 (* RIGHTPAREN *);
  267 (* LBRAC *);
  268 (* RBRAC *);
  269 (* INT *);
  270 (* NOTE *);
  271 (* CHORD *);
  272 (* SCALE *);
  273 (* STANZA *);
  274 (* SCORE *);
  275 (* PLUS *);
  276 (* MINUS *);
  277 (* TIMES *);
  278 (* DIV *);
  279 (* ASSIGN *);
  280 (* VASSIGN *);
  281 (* SEMICOLON *);
  282 (* COMMA *);
    0|]

let yytransl_block = [|
  257 (* INTLITERAL *);
  258 (* OCTAVE *);
  259 (* DURATIONINT *);
  260 (* DURATIONCONST *);
  261 (* STRING *);
  262 (* DATATYPE *);
  263 (* NOTECONST *);
  264 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\004\000\004\000\003\000\003\000\003\000\003\000\003\000\
\003\000\000\000"

let yylen = "\002\000\
\000\000\002\000\003\000\011\000\005\000\011\000\006\000\006\000\
\006\000\000\000\003\000\001\000\001\000\003\000\003\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\010\000\010\000\005\000\
\000\000\010\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\008\000\009\000\000\000\000\000\011\000\000\000\000\000\
\012\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\016\000\017\000\006\000"

let yydgoto = "\002\000\
\003\000\011\000\051\000\035\000"

let yysindex = "\008\000\
\000\000\000\000\005\255\002\255\019\255\021\255\023\255\036\255\
\037\255\038\255\000\000\243\254\006\255\024\255\026\255\027\255\
\030\255\035\255\000\000\012\255\051\255\052\255\053\255\054\255\
\055\255\042\255\040\255\057\255\000\000\000\000\000\000\000\000\
\043\255\000\000\244\254\245\254\246\254\060\255\247\254\000\000\
\062\255\000\000\000\000\045\255\046\255\000\000\003\255\003\255\
\000\000\000\000\014\255\018\255\048\255\003\255\003\255\003\255\
\003\255\049\255\000\000\004\255\004\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\063\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\255\033\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\001\000\230\255"

let yytablesize = 74
let yytable = "\040\000\
\042\000\043\000\045\000\036\000\037\000\049\000\050\000\039\000\
\001\000\012\000\004\000\019\000\026\000\041\000\041\000\041\000\
\041\000\005\000\006\000\007\000\008\000\009\000\010\000\053\000\
\056\000\057\000\013\000\058\000\014\000\020\000\015\000\014\000\
\054\000\055\000\056\000\057\000\054\000\055\000\056\000\057\000\
\014\000\014\000\015\000\016\000\017\000\018\000\033\000\021\000\
\052\000\022\000\023\000\015\000\015\000\024\000\060\000\061\000\
\062\000\063\000\025\000\027\000\028\000\044\000\018\000\029\000\
\030\000\031\000\032\000\034\000\038\000\046\000\047\000\048\000\
\059\000\064\000"

let yycheck = "\012\001\
\012\001\012\001\012\001\030\000\031\000\003\001\004\001\034\000\
\001\000\008\001\006\001\025\001\001\001\026\001\026\001\026\001\
\026\001\013\001\014\001\015\001\016\001\017\001\018\001\010\001\
\021\001\022\001\008\001\010\001\008\001\024\001\008\001\010\001\
\019\001\020\001\021\001\022\001\019\001\020\001\021\001\022\001\
\019\001\020\001\010\001\008\001\008\001\008\001\007\001\024\001\
\048\000\024\001\024\001\019\001\020\001\024\001\054\000\055\000\
\056\000\057\000\024\001\009\001\009\001\002\001\000\000\011\001\
\011\001\011\001\025\001\011\001\026\001\008\001\026\001\026\001\
\025\001\025\001"

let yynames_const = "\
  LEFTPAREN\000\
  RIGHTPAREN\000\
  LBRAC\000\
  RBRAC\000\
  INT\000\
  NOTE\000\
  CHORD\000\
  SCALE\000\
  STANZA\000\
  SCORE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  ASSIGN\000\
  VASSIGN\000\
  SEMICOLON\000\
  COMMA\000\
  "

let yynames_block = "\
  INTLITERAL\000\
  OCTAVE\000\
  DURATIONINT\000\
  DURATIONCONST\000\
  STRING\000\
  DATATYPE\000\
  NOTECONST\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser_new.mly"
( [], [] )
# 176 "parser_new.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 34 "parser_new.mly"
                ( (_2 :: fst _1), snd _1 )
# 184 "parser_new.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 37 "parser_new.mly"
                       ({ vartype = _1; varname = _2})
# 192 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 38 "parser_new.mly"
                                                                                             ( create() )
# 202 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 39 "parser_new.mly"
                                       ( create(_2) )
# 210 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'generic_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 40 "parser_new.mly"
                                                                                                 ( create() )
# 219 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 41 "parser_new.mly"
                                             ( create{} )
# 227 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 42 "parser_new.mly"
                                              ( create{} )
# 235 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 43 "parser_new.mly"
                                             ( create{} )
# 243 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser_new.mly"
 ( [%1] )
# 249 "parser_new.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "parser_new.mly"
                         ( _3 :: _1 )
# 257 "parser_new.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "parser_new.mly"
             ( _1 )
# 264 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser_new.mly"
                 ( _1 )
# 271 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 52 "parser_new.mly"
                                    ( Binop(_1, Add, _3)  )
# 279 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 53 "parser_new.mly"
                                     ( Binop(_1, Sub, _3)  )
# 287 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 54 "parser_new.mly"
                                     ( Binop(_1, Mult, _3)  )
# 295 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 55 "parser_new.mly"
                                   ( Binop(_1, Div, _3)  )
# 303 "parser_new.ml"
               : 'duration_expr))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast_tmp.program)
