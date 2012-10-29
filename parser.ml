type token =
  | INTLITERAL of (int)
  | OCTAVE of (int)
  | DURATIONINT of (int)
  | DURATIONCONST of (string)
  | STRING of (string)
  | DATATYPE of (string)
  | NOTECONST of (string)
  | ID of (string)
  | IN
  | IF
  | ELSE
  | NOELSE
  | WHILE
  | FOREACH
  | ASSIGN
  | PLUSEQ
  | MINUSEQ
  | TIMESEQ
  | DIVIDEEQ
  | MOD
  | MODEQ
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | IS
  | ISNT
  | LT
  | LEQ
  | GT
  | GEQ
  | PLUSPLUS
  | MINUSMINUS
  | SHARP
  | FLAT
  | RAISE
  | LOWER
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
  | VASSIGN
  | SEMICOLON
  | COMMA
  | DOT

open Parsing;;
# 1 "parser.mly"
 open Ast 
# 61 "parser.ml"
let yytransl_const = [|
  265 (* IN *);
  266 (* IF *);
  267 (* ELSE *);
  268 (* NOELSE *);
  269 (* WHILE *);
  270 (* FOREACH *);
  271 (* ASSIGN *);
  272 (* PLUSEQ *);
  273 (* MINUSEQ *);
  274 (* TIMESEQ *);
  275 (* DIVIDEEQ *);
  276 (* MOD *);
  277 (* MODEQ *);
  278 (* PLUS *);
  279 (* MINUS *);
  280 (* TIMES *);
  281 (* DIVIDE *);
  282 (* IS *);
  283 (* ISNT *);
  284 (* LT *);
  285 (* LEQ *);
  286 (* GT *);
  287 (* GEQ *);
  288 (* PLUSPLUS *);
  289 (* MINUSMINUS *);
  290 (* SHARP *);
  291 (* FLAT *);
  292 (* RAISE *);
  293 (* LOWER *);
  294 (* LEFTPAREN *);
  295 (* RIGHTPAREN *);
  296 (* LBRAC *);
  297 (* RBRAC *);
  298 (* INT *);
  299 (* NOTE *);
  300 (* CHORD *);
  301 (* SCALE *);
  302 (* STANZA *);
  303 (* SCORE *);
  304 (* METH *);
  305 (* RETURN *);
  306 (* END *);
  307 (* VASSIGN *);
  308 (* SEMICOLON *);
  309 (* COMMA *);
  310 (* DOT *);
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
\007\000\005\000\005\000\008\000\008\000\008\000\008\000\008\000\
\008\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\013\000\013\000\013\000\013\000\013\000\013\000\012\000\012\000\
\011\000\011\000\011\000\011\000\011\000\011\000\010\000\010\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\014\000\014\000\015\000\
\015\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\002\000\000\000\002\000\002\000\003\000\006\000\008\000\006\000\
\008\000\003\000\011\000\005\000\011\000\006\000\006\000\006\000\
\010\000\004\000\010\000\005\000\005\000\005\000\000\000\003\000\
\001\000\001\000\003\000\003\000\003\000\003\000\000\000\001\000\
\001\000\003\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\004\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\031\000\031\000\031\000\000\000\020\000\000\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\022\000\000\000\023\000\024\000\009\000\010\000\000\000\000\000\
\000\000\032\000\000\000\008\000\000\000\000\000\043\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\011\000\000\000\
\044\000\033\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\042\000\000\000\000\000\000\000\068\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\038\000\019\000\021\000\069\000\
\000\000\026\000\000\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\017\000\000\000\000\000\025\000\
\027\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\044\000\059\000\045\000\046\000\071\000\
\072\000\086\000\076\000\040\000\073\000\118\000\119\000"

let yysindex = "\021\000\
\000\000\000\000\036\255\015\255\041\255\042\255\051\255\079\255\
\082\255\084\255\104\255\000\000\000\000\059\255\061\255\062\255\
\069\255\103\255\134\255\136\255\109\255\000\000\154\255\121\255\
\150\255\149\255\151\255\155\255\152\255\142\255\191\255\179\255\
\000\000\000\000\000\000\218\255\000\000\183\255\000\000\017\255\
\045\255\053\255\229\255\237\255\204\255\000\000\022\000\066\255\
\000\000\250\255\000\000\000\000\000\000\000\000\218\255\226\255\
\228\255\000\000\047\255\000\000\009\255\009\255\000\000\067\255\
\248\255\254\255\015\000\063\255\063\255\000\000\000\000\190\000\
\000\000\000\000\000\000\158\255\221\000\063\255\007\255\018\000\
\063\255\063\255\218\255\001\001\076\001\232\255\063\255\063\255\
\063\255\063\255\063\255\063\255\063\255\063\255\063\255\063\255\
\063\255\063\255\063\255\063\255\063\255\063\255\063\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\255\009\255\
\009\255\009\255\004\000\005\000\076\001\019\000\012\000\014\000\
\004\255\000\000\000\000\026\001\051\001\058\000\000\000\000\000\
\122\001\099\001\099\001\122\001\122\001\116\255\122\001\140\001\
\140\001\116\255\116\255\198\255\198\255\159\000\159\000\159\000\
\159\000\246\254\246\254\000\000\000\000\000\000\000\000\000\000\
\063\255\000\000\017\000\000\000\105\255\055\255\055\255\063\000\
\076\001\071\000\143\255\000\000\249\254\024\000\036\000\028\000\
\030\000\055\255\000\000\000\000\055\255\009\255\009\255\034\000\
\048\000\230\000\144\001\000\000\000\000\049\000\050\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\103\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\072\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\073\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\108\255\
\000\000\000\000\000\000\000\000\064\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\076\000\000\000\000\000\
\000\000\000\000\000\000\000\000\066\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\220\254\000\000\080\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\056\255\050\255\140\255\135\000\212\000\147\255\218\000\244\255\
\016\000\186\255\225\255\123\000\151\000\033\000\061\000\078\000\
\106\000\077\255\246\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\219\255\107\255\
\194\255\000\000\196\255\222\255\000\000\000\000\000\000"

let yytablesize = 695
let yytable = "\041\000\
\042\000\077\000\072\000\170\000\048\000\084\000\085\000\120\000\
\165\000\166\000\155\000\074\000\075\000\113\000\114\000\117\000\
\072\000\060\000\124\000\125\000\176\000\001\000\014\000\177\000\
\129\000\130\000\131\000\132\000\133\000\134\000\135\000\136\000\
\137\000\138\000\139\000\140\000\141\000\142\000\143\000\144\000\
\145\000\004\000\171\000\156\000\121\000\126\000\122\000\063\000\
\015\000\016\000\146\000\147\000\148\000\149\000\064\000\063\000\
\065\000\049\000\017\000\066\000\067\000\073\000\064\000\063\000\
\065\000\046\000\046\000\066\000\067\000\050\000\064\000\045\000\
\045\000\045\000\045\000\073\000\045\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\068\000\051\000\018\000\157\000\
\046\000\019\000\161\000\020\000\068\000\052\000\045\000\069\000\
\070\000\050\000\035\000\035\000\068\000\046\000\046\000\069\000\
\078\000\050\000\057\000\045\000\045\000\021\000\022\000\023\000\
\024\000\178\000\179\000\035\000\029\000\079\000\050\000\025\000\
\080\000\163\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\164\000\041\000\104\000\105\000\106\000\107\000\108\000\
\109\000\026\000\030\000\047\000\047\000\050\000\031\000\041\000\
\041\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
\055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
\055\000\055\000\047\000\111\000\112\000\113\000\114\000\169\000\
\027\000\055\000\028\000\032\000\033\000\036\000\034\000\047\000\
\047\000\037\000\035\000\050\000\115\000\038\000\055\000\055\000\
\053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
\053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
\053\000\092\000\039\000\094\000\095\000\096\000\097\000\043\000\
\053\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\107\000\108\000\109\000\047\000\053\000\053\000\053\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\055\000\058\000\051\000\051\000\051\000\051\000\051\000\054\000\
\051\000\051\000\051\000\036\000\036\000\051\000\051\000\051\000\
\051\000\051\000\051\000\054\000\054\000\054\000\061\000\056\000\
\062\000\123\000\051\000\128\000\036\000\081\000\052\000\052\000\
\052\000\052\000\052\000\082\000\052\000\052\000\052\000\051\000\
\051\000\052\000\052\000\052\000\052\000\052\000\052\000\058\000\
\058\000\058\000\058\000\058\000\083\000\058\000\052\000\150\000\
\151\000\152\000\058\000\058\000\058\000\058\000\058\000\058\000\
\153\000\154\000\160\000\052\000\052\000\162\000\167\000\058\000\
\168\000\172\000\173\000\059\000\059\000\059\000\059\000\059\000\
\174\000\059\000\175\000\180\000\058\000\058\000\059\000\059\000\
\059\000\059\000\059\000\059\000\060\000\060\000\060\000\060\000\
\060\000\181\000\060\000\059\000\184\000\185\000\074\000\060\000\
\060\000\060\000\060\000\060\000\060\000\000\000\005\000\006\000\
\059\000\059\000\070\000\039\000\060\000\040\000\071\000\000\000\
\061\000\061\000\061\000\061\000\061\000\000\000\061\000\000\000\
\000\000\060\000\060\000\061\000\061\000\061\000\061\000\061\000\
\061\000\056\000\056\000\056\000\056\000\056\000\000\000\056\000\
\061\000\000\000\000\000\000\000\056\000\056\000\048\000\048\000\
\048\000\048\000\000\000\048\000\000\000\061\000\061\000\000\000\
\000\000\056\000\000\000\000\000\000\000\057\000\057\000\057\000\
\057\000\057\000\000\000\057\000\000\000\048\000\056\000\056\000\
\057\000\057\000\092\000\000\000\094\000\095\000\096\000\097\000\
\000\000\000\000\048\000\048\000\000\000\057\000\104\000\105\000\
\106\000\107\000\108\000\109\000\000\000\000\000\000\000\000\000\
\000\000\000\000\057\000\057\000\087\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\095\000\096\000\097\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\107\000\108\000\109\000\049\000\049\000\049\000\049\000\000\000\
\049\000\050\000\050\000\050\000\050\000\000\000\050\000\000\000\
\000\000\110\000\111\000\112\000\113\000\114\000\000\000\000\000\
\000\000\000\000\049\000\111\000\112\000\113\000\114\000\000\000\
\050\000\000\000\000\000\116\000\000\000\000\000\000\000\049\000\
\049\000\000\000\000\000\000\000\182\000\050\000\050\000\087\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\109\000\000\000\127\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\108\000\109\000\000\000\
\158\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\109\000\
\000\000\159\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\109\000\087\000\000\000\000\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\109\000\
\087\000\000\000\000\000\000\000\000\000\092\000\000\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\108\000\109\000\092\000\
\000\000\000\000\000\000\096\000\097\000\111\000\112\000\113\000\
\114\000\000\000\000\000\104\000\105\000\106\000\107\000\108\000\
\109\000\000\000\000\000\000\000\000\000\000\000\183\000"

let yycheck = "\034\000\
\035\000\062\000\039\001\011\001\039\000\068\000\069\000\001\001\
\158\000\159\000\007\001\003\001\004\001\024\001\025\001\078\000\
\053\001\055\000\081\000\082\000\170\000\001\000\008\001\173\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\006\001\050\001\040\001\038\001\083\000\040\001\001\001\
\008\001\008\001\111\000\112\000\113\000\114\000\008\001\001\001\
\010\001\041\001\008\001\013\001\014\001\039\001\008\001\001\001\
\010\001\016\001\017\001\013\001\014\001\053\001\008\001\016\001\
\017\001\018\001\019\001\053\001\021\001\042\001\043\001\044\001\
\045\001\046\001\047\001\048\001\038\001\041\001\008\001\122\000\
\039\001\008\001\153\000\008\001\038\001\041\001\039\001\049\001\
\050\001\053\001\022\001\023\001\038\001\052\001\053\001\049\001\
\038\001\053\001\041\001\052\001\053\001\006\001\052\001\051\001\
\051\001\174\000\175\000\039\001\008\001\051\001\053\001\051\001\
\054\001\156\000\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\041\001\039\001\032\001\033\001\034\001\035\001\036\001\
\037\001\051\001\001\001\016\001\017\001\053\001\038\001\052\001\
\053\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\039\001\022\001\023\001\024\001\025\001\041\001\
\051\001\039\001\051\001\038\001\040\001\038\001\040\001\052\001\
\053\001\052\001\040\001\053\001\039\001\007\001\052\001\053\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\020\001\040\001\022\001\023\001\024\001\025\001\006\001\
\039\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\053\001\008\001\052\001\053\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\053\001\008\001\015\001\016\001\017\001\018\001\019\001\039\001\
\021\001\022\001\023\001\022\001\023\001\026\001\027\001\028\001\
\029\001\030\001\031\001\039\001\052\001\053\001\053\001\002\001\
\053\001\008\001\039\001\052\001\039\001\038\001\015\001\016\001\
\017\001\018\001\019\001\038\001\021\001\022\001\023\001\052\001\
\053\001\026\001\027\001\028\001\029\001\030\001\031\001\015\001\
\016\001\017\001\018\001\019\001\038\001\021\001\039\001\052\001\
\052\001\039\001\026\001\027\001\028\001\029\001\030\001\031\001\
\053\001\052\001\009\001\052\001\053\001\053\001\008\001\039\001\
\002\001\050\001\039\001\015\001\016\001\017\001\018\001\019\001\
\053\001\021\001\053\001\050\001\052\001\053\001\026\001\027\001\
\028\001\029\001\030\001\031\001\015\001\016\001\017\001\018\001\
\019\001\050\001\021\001\039\001\052\001\052\001\000\000\026\001\
\027\001\028\001\029\001\030\001\031\001\255\255\039\001\039\001\
\052\001\053\001\039\001\052\001\039\001\052\001\039\001\255\255\
\015\001\016\001\017\001\018\001\019\001\255\255\021\001\255\255\
\255\255\052\001\053\001\026\001\027\001\028\001\029\001\030\001\
\031\001\015\001\016\001\017\001\018\001\019\001\255\255\021\001\
\039\001\255\255\255\255\255\255\026\001\027\001\016\001\017\001\
\018\001\019\001\255\255\021\001\255\255\052\001\053\001\255\255\
\255\255\039\001\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\255\255\021\001\255\255\039\001\052\001\053\001\
\026\001\027\001\020\001\255\255\022\001\023\001\024\001\025\001\
\255\255\255\255\052\001\053\001\255\255\039\001\032\001\033\001\
\034\001\035\001\036\001\037\001\255\255\255\255\255\255\255\255\
\255\255\255\255\052\001\053\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\016\001\017\001\018\001\019\001\255\255\
\021\001\016\001\017\001\018\001\019\001\255\255\021\001\255\255\
\255\255\052\001\022\001\023\001\024\001\025\001\255\255\255\255\
\255\255\255\255\039\001\022\001\023\001\024\001\025\001\255\255\
\039\001\255\255\255\255\039\001\255\255\255\255\255\255\052\001\
\053\001\255\255\255\255\255\255\039\001\052\001\053\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\255\255\039\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\255\255\
\039\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\255\255\039\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\015\001\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\015\001\255\255\255\255\255\255\255\255\020\001\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\020\001\
\255\255\255\255\255\255\024\001\025\001\022\001\023\001\024\001\
\025\001\255\255\255\255\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\255\255\255\255\255\255\255\255\039\001"

let yynames_const = "\
  IN\000\
  IF\000\
  ELSE\000\
  NOELSE\000\
  WHILE\000\
  FOREACH\000\
  ASSIGN\000\
  PLUSEQ\000\
  MINUSEQ\000\
  TIMESEQ\000\
  DIVIDEEQ\000\
  MOD\000\
  MODEQ\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  IS\000\
  ISNT\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  PLUSPLUS\000\
  MINUSMINUS\000\
  SHARP\000\
  FLAT\000\
  RAISE\000\
  LOWER\000\
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
  VASSIGN\000\
  SEMICOLON\000\
  COMMA\000\
  DOT\000\
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
# 70 "parser.mly"
( [], [] )
# 477 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 71 "parser.mly"
                ( (_2 :: fst _1), snd _1 )
# 485 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methdecl) in
    Obj.repr(
# 72 "parser.mly"
                   ( TODO() )
# 493 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'meth_params) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 75 "parser.mly"
                                                                      ( create() )
# 503 "parser.ml"
               : 'methdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
 ( [] )
# 509 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 79 "parser.mly"
              ( List.rev(_1) )
# 516 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 82 "parser.mly"
            ( [_1] )
# 523 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 83 "parser.mly"
                               ( _3 :: _1 )
# 531 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
             ( TODO() )
# 539 "parser.ml"
               : 'param_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
 ( [] )
# 545 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 90 "parser.mly"
                            ( _2 :: _1 )
# 553 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                ( TODO() )
# 560 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 94 "parser.mly"
                             ( Return(_2) )
# 567 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 95 "parser.mly"
                                                           ( If(_3, _5, Block([])) )
# 575 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 96 "parser.mly"
                                                             ( If(_3, _5, _7) )
# 584 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 97 "parser.mly"
                                                 ( While(_3, _5) )
# 592 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'param_decl) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 98 "parser.mly"
                                                               (TODO())
# 601 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 101 "parser.mly"
                       ({ vartype = _1; varname = _2})
# 609 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 102 "parser.mly"
                                                                                             ( TODO() )
# 619 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 103 "parser.mly"
                                       ( create(_2) )
# 627 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'generic_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 104 "parser.mly"
                                                                                                 ( TODO() )
# 636 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 105 "parser.mly"
                                             ( TODO() )
# 644 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 106 "parser.mly"
                                              ( TODO() )
# 652 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 107 "parser.mly"
                                             ( TODO() )
# 660 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 110 "parser.mly"
                                                                                      ( TODO() )
# 670 "parser.ml"
               : 'v_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 111 "parser.mly"
                                   ( TODO() )
# 678 "parser.ml"
               : 'v_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'generic_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 112 "parser.mly"
                                                                                           ( TODO() )
# 687 "parser.ml"
               : 'v_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 113 "parser.mly"
                                       ( TODO() )
# 695 "parser.ml"
               : 'v_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 114 "parser.mly"
                                       ( TODO() )
# 703 "parser.ml"
               : 'v_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 115 "parser.mly"
                                       ( TODO() )
# 711 "parser.ml"
               : 'v_assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
 ( [%1] )
# 717 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
                         ( _3 :: _1 )
# 725 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 122 "parser.mly"
             ( _1 )
# 732 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
                 ( _1 )
# 739 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 124 "parser.mly"
                                    ( Binop(_1, Add, _3)  )
# 747 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 125 "parser.mly"
                                     ( Binop(_1, Sub, _3)  )
# 755 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 126 "parser.mly"
                                     ( Binop(_1, Mult, _3)  )
# 763 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 127 "parser.mly"
                                      ( Binop(_1, Div, _3)  )
# 771 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
 ( NoExpr )
# 777 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
        ( _1 )
# 784 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
    ( Id(_1) )
# 791 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
             ( TODO() )
# 799 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 136 "parser.mly"
              ( TODO() )
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'v_assign) in
    Obj.repr(
# 137 "parser.mly"
            ( TODO() )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                    ( TODO() )
# 821 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                    ( Assign(_1, BinOp(_1, Add, _3)) )
# 829 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                     ( Assign(_1, BinOp(_1, Sub, _3)) )
# 837 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( Assign(_1, BinOp(_1, Mult, _3)) )
# 845 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                      ( Assign(_1, BinOp(_1, Div, _3)) )
# 853 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                   ( Assign(_1, BinOp(_1, Mod, _3)) )
# 861 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                  ( BinOp(_1, Add, _3) )
# 869 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                   ( BinOp(_1, Sub, _3) )
# 877 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                   ( BinOp(_1, Mult, _3) )
# 885 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                    ( BinOp(_1, Div, _3) )
# 893 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                 ( BinOp(_1, Mod, _3) )
# 901 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                ( BinOp(_1, Eq, _3) )
# 909 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
                  ( BinOp(_1, NEq, _3) )
# 917 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
                ( BinOp(_1, Less, _3) )
# 925 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                 ( BinOp(_1, LEq, _3) )
# 933 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                ( BinOp(_1, Greater, _3) )
# 941 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                 ( BinOp(_1, GEq, _3) )
# 949 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
                 ( Assign(_1, BinOp(_1, Add, IntLiteral(1))) )
# 956 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                   ( Assign(_1, BinOp(_1, Sub, IntLiteral(1))) )
# 963 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
              ( TODO() )
# 970 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
             ( TODO() )
# 977 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 159 "parser.mly"
              ( TODO() )
# 984 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
              ( TODO() )
# 991 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
                             ( _2 )
# 998 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 162 "parser.mly"
                                       ( TODO() )
# 1006 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "parser.mly"
 ( [] )
# 1012 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 166 "parser.mly"
                ( List.rev _1 )
# 1019 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "parser.mly"
      ( [_1] )
# 1026 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
                           ( _3 :: _1 )
# 1034 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast_tmp.program)
