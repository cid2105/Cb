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

open Parsing;;
# 1 "parser_tmp.mly"
 open Ast_tmp 
# 71 "parser_tmp.ml"
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
  273 (* SHARP *);
  274 (* FLAT *);
  275 (* ASSIGN *);
  276 (* PEQ *);
  277 (* MEQ *);
  278 (* TEQ *);
  279 (* DIVEQ *);
  280 (* MODEQ *);
  281 (* LOWER *);
  282 (* RAISE *);
  283 (* IS *);
  284 (* ISNT *);
  285 (* LT *);
  286 (* LQT *);
  287 (* GT *);
  288 (* GEQ *);
  289 (* IF *);
  290 (* ELSE *);
  291 (* NOELSE *);
  292 (* ELIF *);
  293 (* FOREACH *);
  294 (* IN *);
  295 (* WHILE *);
  296 (* RETURN *);
  297 (* INT *);
  298 (* NOTE *);
  299 (* CHORD *);
  300 (* SCORE *);
  301 (* STANZA *);
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
  313 (* EIGHT *);
  314 (* SIXTEENTH *);
  318 (* LCOMM *);
  319 (* RCOMM *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  315 (* LITERAL *);
  316 (* ID *);
  317 (* DATATYPE *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\010\000\010\000\011\000\011\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\009\000\000\000\001\000\001\000\003\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\005\000\
\007\000\005\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\002\000\003\000\000\000\000\000\
\011\000\007\000\000\000\000\000\000\000\000\000\009\000\008\000\
\000\000\000\000\010\000\000\000\000\000\000\000\012\000\004\000\
\000\000\000\000\000\000\000\000\019\000\000\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\024\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\000\000\000\000\018\000\000\000\000\000\017\000"

let yydgoto = "\002\000\
\003\000\005\000\006\000\011\000\017\000\020\000\012\000\031\000\
\032\000\057\000\058\000"

let yysindex = "\003\000\
\000\000\000\000\207\254\224\254\000\000\000\000\013\255\227\254\
\000\000\000\000\033\255\042\255\049\255\249\254\000\000\000\000\
\001\255\004\255\000\000\178\255\064\255\079\255\000\000\000\000\
\079\255\079\255\078\255\098\255\000\000\000\255\000\000\006\000\
\005\255\183\255\012\000\012\000\079\255\079\255\079\255\079\255\
\000\000\079\255\079\255\079\255\079\255\079\255\079\255\079\255\
\079\255\079\255\079\255\000\000\000\000\011\255\046\255\012\000\
\070\255\083\255\012\000\246\254\246\254\000\000\000\000\071\255\
\071\255\071\255\071\255\071\255\071\255\228\255\228\255\000\000\
\079\255\069\255\000\000\012\000\228\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\106\000\000\000\000\000\000\000\000\000\107\255\
\000\000\000\000\000\000\108\255\000\000\000\000\000\000\000\000\
\187\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\254\254\000\000\000\000\
\000\000\000\000\039\255\047\255\000\000\000\000\110\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\059\255\
\000\000\114\255\138\255\058\255\086\255\000\000\000\000\093\255\
\100\255\128\255\135\255\142\255\170\255\000\000\000\000\000\000\
\000\000\224\255\000\000\090\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\102\000\000\000\000\000\000\000\103\000\000\000\230\255\
\036\001\000\000\000\000"

let yytablesize = 365
let yytable = "\020\000\
\039\000\044\000\045\000\001\000\020\000\020\000\052\000\020\000\
\020\000\020\000\020\000\004\000\070\000\008\000\042\000\043\000\
\044\000\045\000\040\000\009\000\042\000\043\000\044\000\045\000\
\020\000\020\000\020\000\007\000\020\000\020\000\010\000\046\000\
\047\000\048\000\013\000\049\000\050\000\046\000\047\000\048\000\
\031\000\049\000\050\000\074\000\075\000\031\000\031\000\071\000\
\032\000\014\000\078\000\015\000\016\000\032\000\032\000\042\000\
\043\000\044\000\045\000\021\000\038\000\018\000\020\000\021\000\
\021\000\021\000\038\000\021\000\021\000\051\000\009\000\072\000\
\046\000\047\000\048\000\051\000\049\000\050\000\037\000\022\000\
\042\000\043\000\044\000\045\000\021\000\021\000\021\000\022\000\
\021\000\021\000\073\000\039\000\022\000\022\000\025\000\022\000\
\022\000\039\000\038\000\025\000\025\000\026\000\077\000\025\000\
\026\000\040\000\026\000\026\000\005\000\006\000\051\000\036\000\
\022\000\022\000\022\000\037\000\022\000\022\000\019\000\025\000\
\025\000\025\000\021\000\025\000\025\000\034\000\026\000\026\000\
\026\000\027\000\026\000\026\000\000\000\000\000\027\000\027\000\
\029\000\029\000\030\000\033\000\000\000\029\000\029\000\030\000\
\033\000\033\000\000\000\000\000\030\000\030\000\022\000\000\000\
\000\000\000\000\027\000\027\000\027\000\025\000\027\000\027\000\
\000\000\029\000\029\000\029\000\026\000\029\000\029\000\000\000\
\030\000\030\000\030\000\028\000\030\000\030\000\000\000\000\000\
\028\000\028\000\022\000\000\000\023\000\024\000\000\000\022\000\
\000\000\023\000\053\000\012\000\000\000\012\000\012\000\000\000\
\027\000\000\000\000\000\000\000\028\000\028\000\028\000\029\000\
\028\000\028\000\025\000\026\000\000\000\000\000\030\000\025\000\
\026\000\000\000\027\000\012\000\012\000\000\000\000\000\027\000\
\028\000\000\000\000\000\012\000\000\000\028\000\000\000\000\000\
\016\000\012\000\016\000\016\000\022\000\000\000\023\000\000\000\
\000\000\000\000\028\000\000\000\029\000\030\000\000\000\000\000\
\000\000\029\000\030\000\000\000\000\000\012\000\012\000\000\000\
\016\000\016\000\000\000\000\000\025\000\026\000\000\000\000\000\
\016\000\000\000\000\000\000\000\027\000\000\000\016\000\000\000\
\000\000\000\000\028\000\000\000\041\000\000\000\000\000\042\000\
\043\000\044\000\045\000\000\000\000\000\042\000\043\000\044\000\
\045\000\000\000\016\000\016\000\000\000\000\000\029\000\030\000\
\046\000\047\000\048\000\000\000\049\000\050\000\046\000\047\000\
\048\000\000\000\049\000\050\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\000\000\000\000\035\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\000\000\
\054\000\055\000\056\000\059\000\051\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\076\000"

let yycheck = "\002\001\
\001\001\012\001\013\001\001\000\007\001\008\001\002\001\010\001\
\011\001\012\001\013\001\061\001\002\001\001\001\010\001\011\001\
\012\001\013\001\019\001\007\001\010\001\011\001\012\001\013\001\
\027\001\028\001\029\001\060\001\031\001\032\001\060\001\027\001\
\028\001\029\001\002\001\031\001\032\001\027\001\028\001\029\001\
\002\001\031\001\032\001\070\000\071\000\007\001\008\001\002\001\
\002\001\008\001\077\000\003\001\060\001\007\001\008\001\010\001\
\011\001\012\001\013\001\002\001\002\001\061\001\065\001\060\001\
\007\001\008\001\008\001\010\001\011\001\065\001\007\001\002\001\
\027\001\028\001\029\001\065\001\031\001\032\001\001\001\001\001\
\010\001\011\001\012\001\013\001\027\001\028\001\029\001\002\001\
\031\001\032\001\008\001\002\001\007\001\008\001\002\001\010\001\
\011\001\008\001\001\001\007\001\008\001\002\001\034\001\025\001\
\026\001\000\000\007\001\008\001\002\001\002\001\065\001\002\001\
\027\001\028\001\029\001\002\001\031\001\032\001\017\000\027\001\
\028\001\029\001\065\001\031\001\032\001\023\000\027\001\028\001\
\029\001\002\001\031\001\032\001\255\255\255\255\007\001\008\001\
\002\001\059\001\060\001\002\001\255\255\007\001\008\001\002\001\
\007\001\008\001\255\255\255\255\007\001\008\001\065\001\255\255\
\255\255\255\255\027\001\028\001\029\001\065\001\031\001\032\001\
\255\255\027\001\028\001\029\001\065\001\031\001\032\001\255\255\
\027\001\028\001\029\001\002\001\031\001\032\001\255\255\255\255\
\007\001\008\001\001\001\255\255\003\001\004\001\255\255\001\001\
\255\255\003\001\004\001\001\001\255\255\003\001\004\001\255\255\
\065\001\255\255\255\255\255\255\027\001\028\001\029\001\065\001\
\031\001\032\001\025\001\026\001\255\255\255\255\065\001\025\001\
\026\001\255\255\033\001\025\001\026\001\255\255\255\255\033\001\
\039\001\255\255\255\255\033\001\255\255\039\001\255\255\255\255\
\001\001\039\001\003\001\004\001\001\001\255\255\003\001\255\255\
\255\255\255\255\065\001\255\255\059\001\060\001\255\255\255\255\
\255\255\059\001\060\001\255\255\255\255\059\001\060\001\255\255\
\025\001\026\001\255\255\255\255\025\001\026\001\255\255\255\255\
\033\001\255\255\255\255\255\255\033\001\255\255\039\001\255\255\
\255\255\255\255\039\001\255\255\007\001\255\255\255\255\010\001\
\011\001\012\001\013\001\255\255\255\255\010\001\011\001\012\001\
\013\001\255\255\059\001\060\001\255\255\255\255\059\001\060\001\
\027\001\028\001\029\001\255\255\031\001\032\001\027\001\028\001\
\029\001\255\255\031\001\032\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\022\000\255\255\255\255\025\000\026\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\065\001\255\255\
\037\000\038\000\039\000\040\000\065\001\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\073\000"

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
  SHARP\000\
  FLAT\000\
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
  FOREACH\000\
  IN\000\
  WHILE\000\
  RETURN\000\
  INT\000\
  NOTE\000\
  CHORD\000\
  SCORE\000\
  STANZA\000\
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
  EIGHT\000\
  SIXTEENTH\000\
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
# 373 "parser_tmp.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 34 "parser_tmp.mly"
                  ( (_2 :: fst _1), snd _1 )
# 381 "parser_tmp.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 35 "parser_tmp.mly"
                  ( fst _1, (_2 :: snd _1) )
# 389 "parser_tmp.ml"
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
# 404 "parser_tmp.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser_tmp.mly"
                ( [] )
# 410 "parser_tmp.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 45 "parser_tmp.mly"
                ( List.rev _1 )
# 417 "parser_tmp.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser_tmp.mly"
     ( [_1] )
# 424 "parser_tmp.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser_tmp.mly"
                         ( _3 :: _1 )
# 432 "parser_tmp.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser_tmp.mly"
                   ( [] )
# 438 "parser_tmp.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 53 "parser_tmp.mly"
                     ( _2 :: _1 )
# 446 "parser_tmp.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser_tmp.mly"
                   ( { varname = _2; vartype = _1; value = 0 } )
# 454 "parser_tmp.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser_tmp.mly"
                ( [] )
# 460 "parser_tmp.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser_tmp.mly"
                   ( _2 :: _1 )
# 468 "parser_tmp.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "parser_tmp.mly"
            ( Expr(_1) )
# 475 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 64 "parser_tmp.mly"
                          ( Block(List.rev _2) )
# 482 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser_tmp.mly"
                                            ( If(_3, _5, Block([])) )
# 490 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 66 "parser_tmp.mly"
                                         ( If(_3, _5, _7) )
# 499 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser_tmp.mly"
                                  ( While(_3, _5) )
# 507 "parser_tmp.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser_tmp.mly"
          ( Literal(_1) )
# 514 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser_tmp.mly"
       ( Id(_1) )
# 521 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser_tmp.mly"
                ( Binop(_1, Add, _3) )
# 529 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser_tmp.mly"
                ( Binop(_1, Sub, _3) )
# 537 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser_tmp.mly"
                    ( Binop(_1, Mult, _3) )
# 545 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser_tmp.mly"
                  ( Binop(_1, Div, _3) )
# 553 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser_tmp.mly"
                 ( Binop(_1, Is, _3) )
# 561 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser_tmp.mly"
                   ( Binop(_1, Isnt, _3) )
# 569 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser_tmp.mly"
                 ( Binop(_1, Less, _3) )
# 577 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser_tmp.mly"
                  ( Binop(_1, Leq, _3) )
# 585 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser_tmp.mly"
                 ( Binop(_1, Greater, _3) )
# 593 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser_tmp.mly"
                  ( Binop(_1, Geq, _3) )
# 601 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser_tmp.mly"
                  ( Lower(_2) )
# 608 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser_tmp.mly"
                  ( Raise(_2) )
# 615 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser_tmp.mly"
                   ( Assign(_1, _3) )
# 623 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 85 "parser_tmp.mly"
                                 ( Call(_1, _3) )
# 631 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "parser_tmp.mly"
                       ( _2 )
# 638 "parser_tmp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser_tmp.mly"
                ( [] )
# 644 "parser_tmp.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 90 "parser_tmp.mly"
                 ( List.rev _1 )
# 651 "parser_tmp.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser_tmp.mly"
       ( [_1] )
# 658 "parser_tmp.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser_tmp.mly"
                            ( _3 :: _1 )
# 666 "parser_tmp.ml"
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
