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
  | POUND
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
  | FOR
  | IN
  | WHILE
  | RETURN
  | INT
  | BOOL
  | NOTE
  | CHORD
  | SCORE
  | STANZAS
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
  | LITERAL of (int)
  | ID of (string)
  | DATATYPE of (string)
  | LCOMM
  | RCOMM
  | EOF

open Parsing;;
# 1 "parser_tmp.mly"
 open Ast_tmp 
# 69 "parser_tmp.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRAC *);
  260 (* RBRAC *);
  261 (* LSBRACK *);
  262 (* RSBRACK *);
  263 (* SEMI *);
  264 (* COMMA *);
  265 (* DOT *);
  266 (* P *);
  267 (* M *);
  268 (* TIMES *);
  269 (* DIV *);
  270 (* MOD *);
  271 (* PP *);
  272 (* MM *);
  273 (* POUND *);
  274 (* ASSIGN *);
  275 (* PEQ *);
  276 (* MEQ *);
  277 (* TEQ *);
  278 (* DIVEQ *);
  279 (* MODEQ *);
  280 (* LOWER *);
  281 (* RAISE *);
  282 (* IS *);
  283 (* ISNT *);
  284 (* LT *);
  285 (* LQT *);
  286 (* GT *);
  287 (* GEQ *);
  288 (* IF *);
  289 (* ELSE *);
  290 (* NOELSE *);
  291 (* ELIF *);
  292 (* FOR *);
  293 (* IN *);
  294 (* WHILE *);
  295 (* RETURN *);
  296 (* INT *);
  297 (* BOOL *);
  298 (* NOTE *);
  299 (* CHORD *);
  300 (* SCORE *);
  301 (* STANZAS *);
  302 (* SCALE *);
  303 (* A *);
  304 (* B *);
  305 (* C *);
  306 (* D *);
  307 (* E *);
  308 (* F *);
  309 (* G *);
  310 (* WHOLE *);
  311 (* HALF *);
  312 (* QUARTER *);
  316 (* LCOMM *);
  317 (* RCOMM *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  313 (* LITERAL *);
  314 (* ID *);
  315 (* DATATYPE *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\011\000\011\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\009\000\000\000\001\000\001\000\003\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\005\000\
\007\000\005\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\002\000\003\000\000\000\000\000\
\011\000\007\000\000\000\000\000\000\000\000\000\009\000\008\000\
\000\000\000\000\010\000\000\000\000\000\000\000\012\000\004\000\
\000\000\000\000\019\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\023\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\000\000\000\000\018\000\000\000\
\000\000\017\000"

let yydgoto = "\002\000\
\003\000\005\000\006\000\011\000\017\000\020\000\012\000\029\000\
\030\000\053\000\054\000"

let yysindex = "\011\000\
\000\000\000\000\198\254\212\254\000\000\000\000\043\255\220\254\
\000\000\000\000\021\255\026\255\042\255\246\254\000\000\000\000\
\248\254\002\255\000\000\159\255\060\255\012\255\000\000\000\000\
\048\255\077\255\000\000\001\255\000\000\246\255\005\255\182\255\
\012\255\012\255\012\255\012\255\000\000\012\255\012\255\012\255\
\012\255\012\255\012\255\012\255\012\255\012\255\012\255\000\000\
\000\000\028\255\062\255\252\255\082\255\079\255\252\255\247\254\
\247\254\000\000\000\000\135\255\135\255\135\255\135\255\135\255\
\135\255\193\255\193\255\000\000\012\255\061\255\000\000\252\255\
\193\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\101\000\000\000\000\000\000\000\000\000\100\255\
\000\000\000\000\000\000\104\255\000\000\000\000\000\000\000\000\
\189\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\254\254\000\000\000\000\000\000\000\000\
\000\000\000\000\105\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\044\255\000\000\106\255\035\255\055\255\
\069\255\000\000\000\000\096\255\103\255\112\255\142\255\150\255\
\180\255\000\000\000\000\000\000\000\000\197\255\000\000\045\255\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\092\000\000\000\000\000\000\000\089\000\000\000\210\255\
\253\000\000\000\000\000"

let yytablesize = 322
let yytable = "\020\000\
\004\000\035\000\040\000\041\000\020\000\020\000\048\000\020\000\
\020\000\020\000\020\000\001\000\022\000\007\000\038\000\039\000\
\040\000\041\000\036\000\070\000\071\000\010\000\013\000\020\000\
\020\000\020\000\074\000\020\000\020\000\066\000\042\000\043\000\
\044\000\014\000\045\000\046\000\031\000\038\000\039\000\040\000\
\041\000\031\000\031\000\008\000\015\000\036\000\037\000\016\000\
\033\000\009\000\018\000\036\000\037\000\042\000\043\000\044\000\
\021\000\045\000\046\000\021\000\020\000\021\000\021\000\067\000\
\021\000\021\000\009\000\047\000\027\000\028\000\022\000\038\000\
\039\000\040\000\041\000\022\000\022\000\034\000\022\000\022\000\
\021\000\021\000\021\000\068\000\021\000\021\000\069\000\042\000\
\043\000\044\000\047\000\045\000\046\000\073\000\022\000\022\000\
\022\000\025\000\022\000\022\000\038\000\005\000\025\000\025\000\
\026\000\006\000\034\000\035\000\019\000\026\000\026\000\032\000\
\000\000\027\000\000\000\000\000\000\000\021\000\027\000\027\000\
\000\000\025\000\025\000\025\000\047\000\025\000\025\000\000\000\
\026\000\026\000\026\000\022\000\026\000\026\000\000\000\000\000\
\000\000\027\000\027\000\027\000\000\000\027\000\027\000\029\000\
\038\000\039\000\040\000\041\000\029\000\029\000\000\000\030\000\
\000\000\000\000\000\000\000\000\030\000\030\000\025\000\022\000\
\000\000\023\000\024\000\000\000\000\000\026\000\000\000\029\000\
\029\000\029\000\000\000\029\000\029\000\000\000\027\000\030\000\
\030\000\030\000\000\000\030\000\030\000\028\000\022\000\000\000\
\023\000\049\000\028\000\028\000\000\000\012\000\025\000\012\000\
\012\000\022\000\000\000\023\000\026\000\016\000\000\000\016\000\
\016\000\000\000\000\000\000\000\029\000\028\000\028\000\028\000\
\000\000\028\000\028\000\000\000\030\000\025\000\000\000\027\000\
\028\000\000\000\000\000\026\000\012\000\000\000\000\000\000\000\
\025\000\000\000\012\000\000\000\016\000\000\000\026\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\027\000\028\000\
\000\000\000\000\028\000\000\000\000\000\012\000\012\000\000\000\
\000\000\027\000\028\000\000\000\037\000\016\000\016\000\038\000\
\039\000\040\000\041\000\000\000\000\000\038\000\039\000\040\000\
\041\000\000\000\000\000\000\000\000\000\000\000\000\000\042\000\
\043\000\044\000\031\000\045\000\046\000\042\000\043\000\044\000\
\000\000\045\000\046\000\000\000\000\000\050\000\051\000\052\000\
\055\000\000\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\000\000\000\000\
\000\000\000\000\047\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\000"

let yycheck = "\002\001\
\059\001\001\001\012\001\013\001\007\001\008\001\002\001\010\001\
\011\001\012\001\013\001\001\000\001\001\058\001\010\001\011\001\
\012\001\013\001\018\001\066\000\067\000\058\001\002\001\026\001\
\027\001\028\001\073\000\030\001\031\001\002\001\026\001\027\001\
\028\001\008\001\030\001\031\001\002\001\010\001\011\001\012\001\
\013\001\007\001\008\001\001\001\003\001\002\001\002\001\058\001\
\001\001\007\001\059\001\008\001\008\001\026\001\027\001\028\001\
\002\001\030\001\031\001\058\001\063\001\007\001\008\001\002\001\
\010\001\011\001\007\001\063\001\057\001\058\001\002\001\010\001\
\011\001\012\001\013\001\007\001\008\001\001\001\010\001\011\001\
\026\001\027\001\028\001\002\001\030\001\031\001\008\001\026\001\
\027\001\028\001\063\001\030\001\031\001\033\001\026\001\027\001\
\028\001\002\001\030\001\031\001\000\000\002\001\007\001\008\001\
\002\001\002\001\002\001\002\001\017\000\007\001\008\001\023\000\
\255\255\002\001\255\255\255\255\255\255\063\001\007\001\008\001\
\255\255\026\001\027\001\028\001\063\001\030\001\031\001\255\255\
\026\001\027\001\028\001\063\001\030\001\031\001\255\255\255\255\
\255\255\026\001\027\001\028\001\255\255\030\001\031\001\002\001\
\010\001\011\001\012\001\013\001\007\001\008\001\255\255\002\001\
\255\255\255\255\255\255\255\255\007\001\008\001\063\001\001\001\
\255\255\003\001\004\001\255\255\255\255\063\001\255\255\026\001\
\027\001\028\001\255\255\030\001\031\001\255\255\063\001\026\001\
\027\001\028\001\255\255\030\001\031\001\002\001\001\001\255\255\
\003\001\004\001\007\001\008\001\255\255\001\001\032\001\003\001\
\004\001\001\001\255\255\003\001\038\001\001\001\255\255\003\001\
\004\001\255\255\255\255\255\255\063\001\026\001\027\001\028\001\
\255\255\030\001\031\001\255\255\063\001\032\001\255\255\057\001\
\058\001\255\255\255\255\038\001\032\001\255\255\255\255\255\255\
\032\001\255\255\038\001\255\255\032\001\255\255\038\001\255\255\
\255\255\255\255\038\001\255\255\255\255\255\255\057\001\058\001\
\255\255\255\255\063\001\255\255\255\255\057\001\058\001\255\255\
\255\255\057\001\058\001\255\255\007\001\057\001\058\001\010\001\
\011\001\012\001\013\001\255\255\255\255\010\001\011\001\012\001\
\013\001\255\255\255\255\255\255\255\255\255\255\255\255\026\001\
\027\001\028\001\022\000\030\001\031\001\026\001\027\001\028\001\
\255\255\030\001\031\001\255\255\255\255\033\000\034\000\035\000\
\036\000\255\255\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\063\001\255\255\255\255\255\255\
\255\255\255\255\063\001\255\255\255\255\255\255\255\255\255\255\
\255\255\069\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRAC\000\
  RBRAC\000\
  LSBRACK\000\
  RSBRACK\000\
  SEMI\000\
  COMMA\000\
  DOT\000\
  P\000\
  M\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  PP\000\
  MM\000\
  POUND\000\
  ASSIGN\000\
  PEQ\000\
  MEQ\000\
  TEQ\000\
  DIVEQ\000\
  MODEQ\000\
  LOWER\000\
  RAISE\000\
  IS\000\
  ISNT\000\
  LT\000\
  LQT\000\
  GT\000\
  GEQ\000\
  IF\000\
  ELSE\000\
  NOELSE\000\
  ELIF\000\
  FOR\000\
  IN\000\
  WHILE\000\
  RETURN\000\
  INT\000\
  BOOL\000\
  NOTE\000\
  CHORD\000\
  SCORE\000\
  STANZAS\000\
  SCALE\000\
  A\000\
  B\000\
  C\000\
  D\000\
  E\000\
  F\000\
  G\000\
  WHOLE\000\
  HALF\000\
  QUARTER\000\
  LCOMM\000\
  RCOMM\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  DATATYPE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser_tmp.mly"
                ( [], [] )
# 357 "parser_tmp.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 34 "parser_tmp.mly"
                  ( (_2 :: fst _1), snd _1 )
# 365 "parser_tmp.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 35 "parser_tmp.mly"
                  ( fst _1, (_2 :: snd _1) )
# 373 "parser_tmp.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 38 "parser_tmp.mly"
            ( { fname = _2; 
                rettype = _1; 
                formals = _4; 
                locals = List.rev _7; 
                body = List.rev _8 } )
# 388 "parser_tmp.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser_tmp.mly"
                ( [] )
# 394 "parser_tmp.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 45 "parser_tmp.mly"
                ( List.rev _1 )
# 401 "parser_tmp.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser_tmp.mly"
     ( [_1] )
# 408 "parser_tmp.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser_tmp.mly"
                         ( _3 :: _1 )
# 416 "parser_tmp.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser_tmp.mly"
                   ( [] )
# 422 "parser_tmp.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 53 "parser_tmp.mly"
                     ( _2 :: _1 )
# 430 "parser_tmp.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser_tmp.mly"
                   ( { varname = _2; vartype = _1; value = 0 } )
# 438 "parser_tmp.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser_tmp.mly"
                ( [] )
# 444 "parser_tmp.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser_tmp.mly"
                   ( _2 :: _1 )
# 452 "parser_tmp.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "parser_tmp.mly"
            ( Expr(_1) )
# 459 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 64 "parser_tmp.mly"
                          ( Block(List.rev _2) )
# 466 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser_tmp.mly"
                                            ( If(_3, _5, Block([])) )
# 474 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 66 "parser_tmp.mly"
                                         ( If(_3, _5, _7) )
# 483 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser_tmp.mly"
                                  ( While(_3, _5) )
# 491 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser_tmp.mly"
          ( Literal(_1) )
# 498 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser_tmp.mly"
       ( Id(_1) )
# 505 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser_tmp.mly"
                ( Binop(_1, Add, _3) )
# 513 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser_tmp.mly"
                ( Binop(_1, Sub, _3) )
# 521 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser_tmp.mly"
                    ( Binop(_1, Mult, _3) )
# 529 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser_tmp.mly"
                  ( Binop(_1, Div, _3) )
# 537 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser_tmp.mly"
                 ( Binop(_1, Equal, _3) )
# 545 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser_tmp.mly"
                   ( Binop(_1, Neq, _3) )
# 553 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser_tmp.mly"
                 ( Binop(_1, Less, _3) )
# 561 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser_tmp.mly"
                  ( Binop(_1, Leq, _3) )
# 569 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser_tmp.mly"
                 ( Binop(_1, Greater, _3) )
# 577 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser_tmp.mly"
                  ( Binop(_1, Geq, _3) )
# 585 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser_tmp.mly"
                   ( Assign(_1, _3) )
# 593 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 83 "parser_tmp.mly"
                                 ( Call(_1, _3) )
# 601 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 84 "parser_tmp.mly"
                       ( _2 )
# 608 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser_tmp.mly"
                ( [] )
# 614 "parser_tmp.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 88 "parser_tmp.mly"
                 ( List.rev _1 )
# 621 "parser_tmp.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser_tmp.mly"
       ( [_1] )
# 628 "parser_tmp.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser_tmp.mly"
                            ( _3 :: _1 )
# 636 "parser_tmp.ml"
               : 'actuals_list))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
