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
  | METH
  | RETURN
  | END
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
# 37 "parser_new.ml"
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
  275 (* METH *);
  276 (* RETURN *);
  277 (* END *);
  278 (* PLUS *);
  279 (* MINUS *);
  280 (* TIMES *);
  281 (* DIV *);
  282 (* ASSIGN *);
  283 (* VASSIGN *);
  284 (* SEMICOLON *);
  285 (* COMMA *);
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
\001\000\001\000\001\000\003\000\004\000\004\000\006\000\006\000\
\007\000\005\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\009\000\009\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\002\000\001\000\003\000\011\000\005\000\011\000\006\000\006\000\
\006\000\000\000\003\000\001\000\001\000\003\000\003\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\018\000\018\000\000\000\013\000\000\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\015\000\000\000\016\000\017\000\009\000\000\000\000\000\000\000\
\000\000\019\000\010\000\000\000\008\000\000\000\000\000\004\000\
\020\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\000\000\024\000\025\000\014\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\044\000\060\000\045\000\046\000\067\000\
\040\000"

let yysindex = "\003\000\
\000\000\000\000\023\255\006\255\007\255\008\255\022\255\025\255\
\027\255\040\255\026\255\000\000\000\000\028\255\030\255\031\255\
\032\255\033\255\034\255\035\255\041\255\000\000\049\255\042\255\
\054\255\053\255\055\255\056\255\059\255\037\255\062\255\060\255\
\000\000\000\000\000\000\064\255\000\000\043\255\000\000\244\254\
\245\254\246\254\065\255\066\255\045\255\000\000\073\255\247\254\
\000\000\069\255\000\000\000\000\000\000\070\255\064\255\050\255\
\051\255\000\000\000\000\061\255\000\000\004\255\004\255\000\000\
\000\000\000\000\255\254\003\255\057\255\004\255\004\255\004\255\
\004\255\058\255\000\000\243\254\243\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\081\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\074\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\077\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\255\024\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\238\255\
\227\255"

let yytablesize = 87
let yytable = "\049\000\
\051\000\052\000\057\000\001\000\041\000\042\000\065\000\066\000\
\069\000\048\000\072\000\073\000\074\000\014\000\015\000\016\000\
\050\000\050\000\050\000\050\000\070\000\071\000\072\000\073\000\
\070\000\071\000\072\000\073\000\004\000\017\000\022\000\021\000\
\018\000\023\000\019\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\022\000\022\000\068\000\023\000\023\000\020\000\
\029\000\030\000\031\000\076\000\077\000\078\000\079\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\032\000\033\000\
\037\000\034\000\035\000\036\000\038\000\043\000\039\000\047\000\
\053\000\055\000\056\000\054\000\058\000\059\000\062\000\063\000\
\026\000\064\000\061\000\005\000\075\000\080\000\006\000"

let yycheck = "\012\001\
\012\001\012\001\012\001\001\000\034\000\035\000\003\001\004\001\
\010\001\039\000\024\001\025\001\010\001\008\001\008\001\008\001\
\029\001\029\001\029\001\029\001\022\001\023\001\024\001\025\001\
\022\001\023\001\024\001\025\001\006\001\008\001\010\001\006\001\
\008\001\010\001\008\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\022\001\023\001\063\000\022\001\023\001\008\001\
\008\001\001\001\009\001\070\000\071\000\072\000\073\000\028\001\
\027\001\027\001\027\001\027\001\027\001\027\001\009\001\011\001\
\028\001\011\001\011\001\009\001\007\001\006\001\011\001\029\001\
\008\001\029\001\002\001\010\001\008\001\008\001\029\001\029\001\
\000\000\021\001\055\000\010\001\028\001\028\001\010\001"

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
  METH\000\
  RETURN\000\
  END\000\
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
# 197 "parser_new.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 34 "parser_new.mly"
                ( (_2 :: fst _1), snd _1 )
# 205 "parser_new.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methdecl) in
    Obj.repr(
# 35 "parser_new.mly"
                   ( TODO() )
# 213 "parser_new.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'meth_params) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 38 "parser_new.mly"
                                                                      ( create() )
# 223 "parser_new.ml"
               : 'methdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser_new.mly"
 ( [] )
# 229 "parser_new.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 42 "parser_new.mly"
              ( List.rev(_1) )
# 236 "parser_new.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 45 "parser_new.mly"
            ( [_1] )
# 243 "parser_new.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 46 "parser_new.mly"
                               ( _3 :: _1 )
# 251 "parser_new.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser_new.mly"
  ( {	create{} } )
# 259 "parser_new.ml"
               : 'param_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser_new.mly"
    ( create{} )
# 266 "parser_new.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser_new.mly"
                       ({ vartype = _1; varname = _2})
# 274 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 57 "parser_new.mly"
                                                                                             ( create() )
# 284 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 58 "parser_new.mly"
                                       ( create(_2) )
# 292 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'generic_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 59 "parser_new.mly"
                                                                                                 ( create() )
# 301 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 60 "parser_new.mly"
                                             ( create{} )
# 309 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 61 "parser_new.mly"
                                              ( create{} )
# 317 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 62 "parser_new.mly"
                                             ( create{} )
# 325 "parser_new.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser_new.mly"
 ( [%1] )
# 331 "parser_new.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser_new.mly"
                         ( _3 :: _1 )
# 339 "parser_new.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "parser_new.mly"
             ( _1 )
# 346 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser_new.mly"
                 ( _1 )
# 353 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 71 "parser_new.mly"
                                    ( Binop(_1, Add, _3)  )
# 361 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 72 "parser_new.mly"
                                     ( Binop(_1, Sub, _3)  )
# 369 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 73 "parser_new.mly"
                                     ( Binop(_1, Mult, _3)  )
# 377 "parser_new.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 74 "parser_new.mly"
                                   ( Binop(_1, Div, _3)  )
# 385 "parser_new.ml"
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
