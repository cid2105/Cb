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
\011\000\011\000\011\000\010\000\010\000\010\000\010\000\010\000\
\010\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\012\000\012\000\013\000\
\013\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\002\000\000\000\002\000\002\000\003\000\006\000\008\000\006\000\
\008\000\003\000\011\000\005\000\011\000\006\000\006\000\006\000\
\000\000\003\000\005\000\001\000\001\000\003\000\003\000\003\000\
\003\000\001\000\003\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\004\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\025\000\025\000\000\000\020\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\022\000\000\000\023\000\024\000\009\000\010\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\011\000\
\000\000\028\000\029\000\000\000\000\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\054\000\055\000\
\056\000\057\000\058\000\059\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\000\000\000\000\000\000\060\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\033\000\019\000\021\000\061\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\016\000\000\000\
\000\000\000\000\015\000\017\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\044\000\059\000\045\000\046\000\072\000\
\073\000\076\000\040\000\118\000\119\000"

let yysindex = "\008\000\
\000\000\000\000\030\255\029\255\058\255\085\255\113\255\116\255\
\118\255\122\255\121\255\000\000\000\000\070\255\072\255\074\255\
\106\255\108\255\109\255\110\255\124\255\000\000\161\255\125\255\
\128\255\150\255\168\255\188\255\189\255\177\255\231\255\199\255\
\000\000\000\000\000\000\240\255\000\000\212\255\000\000\219\254\
\220\254\078\255\011\000\237\255\224\255\000\000\026\000\114\255\
\000\000\029\000\000\000\000\000\000\000\000\000\240\255\241\255\
\242\255\016\000\033\255\000\000\036\255\036\255\046\000\000\000\
\000\255\018\000\019\000\020\000\048\255\048\255\000\000\000\000\
\192\000\000\000\000\000\040\255\045\255\000\000\048\255\048\255\
\051\000\048\255\048\255\240\255\010\001\229\000\048\255\048\255\
\048\255\048\255\048\255\048\255\048\255\048\255\048\255\048\255\
\048\255\048\255\048\255\048\255\048\255\048\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\036\255\036\255\036\255\
\036\255\014\000\023\000\120\001\082\001\037\000\024\000\000\000\
\034\001\058\001\069\000\000\000\000\000\102\001\102\001\120\001\
\120\001\055\255\120\001\165\000\165\000\055\255\055\255\169\255\
\169\255\138\001\138\001\138\001\138\001\020\255\020\255\000\000\
\000\000\000\000\000\000\000\000\048\255\047\255\047\255\077\000\
\082\001\248\254\047\000\057\000\047\255\000\000\000\000\047\255\
\050\000\052\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\103\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\074\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\154\255\000\000\000\000\000\000\000\000\000\000\000\000\
\081\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\076\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\110\000\115\255\000\000\080\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\255\112\255\116\000\
\135\000\119\255\154\000\214\255\233\255\157\255\195\255\072\000\
\091\000\252\255\015\000\034\000\053\000\130\255\235\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\117\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\207\255\200\000\
\187\255\196\255\229\255\000\000\000\000"

let yytablesize = 687
let yytable = "\085\000\
\086\000\077\000\157\000\049\000\051\000\060\000\041\000\042\000\
\001\000\116\000\117\000\048\000\121\000\122\000\079\000\050\000\
\050\000\126\000\127\000\128\000\129\000\130\000\131\000\132\000\
\133\000\134\000\135\000\136\000\137\000\138\000\139\000\140\000\
\141\000\064\000\123\000\004\000\014\000\080\000\074\000\075\000\
\065\000\158\000\066\000\112\000\113\000\067\000\068\000\064\000\
\064\000\142\000\143\000\144\000\145\000\081\000\065\000\065\000\
\066\000\038\000\038\000\067\000\068\000\110\000\111\000\112\000\
\113\000\015\000\110\000\111\000\112\000\113\000\069\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\114\000\153\000\
\038\000\070\000\071\000\115\000\069\000\069\000\103\000\104\000\
\105\000\106\000\107\000\108\000\016\000\038\000\038\000\070\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\052\000\034\000\
\017\000\022\000\023\000\018\000\024\000\019\000\021\000\039\000\
\039\000\020\000\050\000\029\000\034\000\034\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\039\000\030\000\
\030\000\064\000\057\000\065\000\025\000\047\000\026\000\027\000\
\028\000\030\000\031\000\039\000\039\000\032\000\050\000\064\000\
\030\000\065\000\047\000\047\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\091\000\033\000\093\000\094\000\
\095\000\096\000\026\000\045\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\108\000\026\000\034\000\
\045\000\045\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\036\000\035\000\037\000\043\000\043\000\043\000\
\043\000\046\000\043\000\043\000\043\000\038\000\039\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\046\000\046\000\
\044\000\044\000\044\000\044\000\043\000\044\000\044\000\044\000\
\031\000\031\000\044\000\044\000\044\000\044\000\044\000\044\000\
\047\000\043\000\043\000\050\000\050\000\050\000\050\000\044\000\
\050\000\031\000\053\000\054\000\055\000\050\000\050\000\050\000\
\050\000\050\000\050\000\056\000\044\000\044\000\051\000\051\000\
\051\000\051\000\050\000\051\000\058\000\061\000\062\000\063\000\
\051\000\051\000\051\000\051\000\051\000\051\000\078\000\050\000\
\050\000\052\000\052\000\052\000\052\000\051\000\052\000\082\000\
\083\000\084\000\120\000\052\000\052\000\052\000\052\000\052\000\
\052\000\146\000\051\000\051\000\053\000\053\000\053\000\053\000\
\052\000\053\000\147\000\148\000\149\000\152\000\053\000\053\000\
\053\000\053\000\053\000\053\000\156\000\052\000\052\000\048\000\
\048\000\048\000\048\000\053\000\048\000\154\000\155\000\160\000\
\159\000\048\000\048\000\163\000\161\000\164\000\066\000\162\000\
\053\000\053\000\049\000\049\000\049\000\049\000\048\000\049\000\
\005\000\006\000\062\000\000\000\049\000\049\000\063\000\000\000\
\000\000\000\000\000\000\048\000\048\000\037\000\037\000\037\000\
\037\000\049\000\037\000\040\000\040\000\040\000\040\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\049\000\049\000\
\000\000\000\000\000\000\000\000\037\000\000\000\041\000\041\000\
\041\000\041\000\040\000\041\000\000\000\000\000\000\000\000\000\
\000\000\037\000\037\000\000\000\000\000\000\000\000\000\040\000\
\040\000\042\000\042\000\042\000\042\000\041\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\091\000\000\000\041\000\041\000\095\000\096\000\000\000\000\000\
\042\000\000\000\000\000\000\000\103\000\104\000\105\000\106\000\
\107\000\108\000\000\000\000\000\000\000\042\000\042\000\087\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\109\000\087\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\095\000\096\000\097\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\107\000\108\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\125\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\000\000\
\124\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\000\000\
\150\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\000\000\
\151\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\096\000\097\000\
\098\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\107\000\108\000\091\000\000\000\093\000\094\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\091\000\000\000\093\000\
\094\000\095\000\096\000\000\000\000\000\000\000\000\000\000\000\
\000\000\103\000\104\000\105\000\106\000\107\000\108\000"

let yycheck = "\069\000\
\070\000\062\000\011\001\041\001\041\001\055\000\034\000\035\000\
\001\000\079\000\080\000\039\000\082\000\083\000\015\001\053\001\
\053\001\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\001\001\084\000\006\001\008\001\038\001\003\001\004\001\
\008\001\050\001\010\001\024\001\025\001\013\001\014\001\001\001\
\001\001\110\000\111\000\112\000\113\000\054\001\008\001\008\001\
\010\001\016\001\017\001\013\001\014\001\022\001\023\001\024\001\
\025\001\008\001\022\001\023\001\024\001\025\001\038\001\042\001\
\043\001\044\001\045\001\046\001\047\001\048\001\039\001\149\000\
\039\001\049\001\050\001\039\001\038\001\038\001\032\001\033\001\
\034\001\035\001\036\001\037\001\008\001\052\001\053\001\049\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\041\001\039\001\
\008\001\052\001\051\001\008\001\051\001\008\001\006\001\016\001\
\017\001\008\001\053\001\008\001\052\001\053\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\039\001\022\001\
\023\001\039\001\041\001\039\001\051\001\039\001\051\001\051\001\
\051\001\001\001\038\001\052\001\053\001\038\001\053\001\053\001\
\039\001\053\001\052\001\053\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\020\001\040\001\022\001\023\001\
\024\001\025\001\041\001\039\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\053\001\040\001\
\052\001\053\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\038\001\040\001\052\001\016\001\017\001\018\001\
\019\001\039\001\021\001\022\001\023\001\007\001\040\001\026\001\
\027\001\028\001\029\001\030\001\031\001\006\001\052\001\053\001\
\016\001\017\001\018\001\019\001\039\001\021\001\022\001\023\001\
\022\001\023\001\026\001\027\001\028\001\029\001\030\001\031\001\
\053\001\052\001\053\001\016\001\017\001\018\001\019\001\039\001\
\021\001\039\001\008\001\039\001\053\001\026\001\027\001\028\001\
\029\001\030\001\031\001\002\001\052\001\053\001\016\001\017\001\
\018\001\019\001\039\001\021\001\008\001\053\001\053\001\024\001\
\026\001\027\001\028\001\029\001\030\001\031\001\001\001\052\001\
\053\001\016\001\017\001\018\001\019\001\039\001\021\001\038\001\
\038\001\038\001\008\001\026\001\027\001\028\001\029\001\030\001\
\031\001\052\001\052\001\053\001\016\001\017\001\018\001\019\001\
\039\001\021\001\052\001\039\001\053\001\009\001\026\001\027\001\
\028\001\029\001\030\001\031\001\008\001\052\001\053\001\016\001\
\017\001\018\001\019\001\039\001\021\001\150\000\151\000\039\001\
\050\001\026\001\027\001\050\001\157\000\050\001\000\000\160\000\
\052\001\053\001\016\001\017\001\018\001\019\001\039\001\021\001\
\039\001\039\001\039\001\255\255\026\001\027\001\039\001\255\255\
\255\255\255\255\255\255\052\001\053\001\016\001\017\001\018\001\
\019\001\039\001\021\001\016\001\017\001\018\001\019\001\255\255\
\021\001\255\255\255\255\255\255\255\255\255\255\052\001\053\001\
\255\255\255\255\255\255\255\255\039\001\255\255\016\001\017\001\
\018\001\019\001\039\001\021\001\255\255\255\255\255\255\255\255\
\255\255\052\001\053\001\255\255\255\255\255\255\255\255\052\001\
\053\001\016\001\017\001\018\001\019\001\039\001\021\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\020\001\255\255\052\001\053\001\024\001\025\001\255\255\255\255\
\039\001\255\255\255\255\255\255\032\001\033\001\034\001\035\001\
\036\001\037\001\255\255\255\255\255\255\052\001\053\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\052\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\052\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\255\255\
\039\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\255\255\
\039\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\255\255\
\039\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\020\001\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\020\001\255\255\022\001\
\023\001\024\001\025\001\255\255\255\255\255\255\255\255\255\255\
\255\255\032\001\033\001\034\001\035\001\036\001\037\001"

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
# 464 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 71 "parser.mly"
                ( (_2 :: fst _1), snd _1 )
# 472 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methdecl) in
    Obj.repr(
# 72 "parser.mly"
                   ( TODO() )
# 480 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'meth_params) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 75 "parser.mly"
                                                                      ( create() )
# 490 "parser.ml"
               : 'methdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
 ( [] )
# 496 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 79 "parser.mly"
              ( List.rev(_1) )
# 503 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 82 "parser.mly"
            ( [_1] )
# 510 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 83 "parser.mly"
                               ( _3 :: _1 )
# 518 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
             ( TODO() )
# 526 "parser.ml"
               : 'param_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
 ( [] )
# 532 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 90 "parser.mly"
                            ( _2 :: _1 )
# 540 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                ( TODO() )
# 547 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                         ( Return(_2) )
# 554 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 95 "parser.mly"
                                                           ( If(_3, _5, Block([])) )
# 562 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 96 "parser.mly"
                                                             ( If(_3, _5, _7) )
# 571 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 97 "parser.mly"
                                                 ( While(_3, _5) )
# 579 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'param_decl) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 98 "parser.mly"
                                                               (TODO())
# 588 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 101 "parser.mly"
                       ({ vartype = _1; varname = _2})
# 596 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 102 "parser.mly"
                                                                                             ( TODO() )
# 606 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 103 "parser.mly"
                                       ( create(_2) )
# 614 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'generic_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 104 "parser.mly"
                                                                                                 ( TODO() )
# 623 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 105 "parser.mly"
                                             ( TODO() )
# 631 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 106 "parser.mly"
                                              ( TODO() )
# 639 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 107 "parser.mly"
                                             ( TODO() )
# 647 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
 ( [%1] )
# 653 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
                         ( _3 :: _1 )
# 661 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
                                          ( TODO() )
# 670 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 115 "parser.mly"
             ( _1 )
# 677 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                 ( _1 )
# 684 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 117 "parser.mly"
                                    ( Binop(_1, Add, _3)  )
# 692 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 118 "parser.mly"
                                     ( Binop(_1, Sub, _3)  )
# 700 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 119 "parser.mly"
                                     ( Binop(_1, Mult, _3)  )
# 708 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 120 "parser.mly"
                                      ( Binop(_1, Div, _3)  )
# 716 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
    ( Id(_1) )
# 723 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "parser.mly"
             ( TODO() )
# 731 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 125 "parser.mly"
              ( TODO() )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                  ( TODO() )
# 746 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                    ( Assign(_1, BinOp(_1, Add, _3)) )
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( Assign(_1, BinOp(_1, Sub, _3)) )
# 762 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                     ( Assign(_1, BinOp(_1, Mult, _3)) )
# 770 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                      ( Assign(_1, BinOp(_1, Div, _3)) )
# 778 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                   ( Assign(_1, BinOp(_1, Mod, _3)) )
# 786 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                  ( BinOp(_1, Add, _3) )
# 794 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                   ( BinOp(_1, Sub, _3) )
# 802 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                   ( BinOp(_1, Mult, _3) )
# 810 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                    ( BinOp(_1, Div, _3) )
# 818 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                 ( BinOp(_1, Mod, _3) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                ( BinOp(_1, Eq, _3) )
# 834 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                  ( BinOp(_1, NEq, _3) )
# 842 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                ( BinOp(_1, Less, _3) )
# 850 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                 ( BinOp(_1, LEq, _3) )
# 858 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                ( BinOp(_1, Greater, _3) )
# 866 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                 ( BinOp(_1, GEq, _3) )
# 874 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                 ( Assign(_1, BinOp(_1, Add, IntLiteral(1))) )
# 881 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                   ( Assign(_1, BinOp(_1, Sub, IntLiteral(1))) )
# 888 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
              ( TODO() )
# 895 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
             ( TODO() )
# 902 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
              ( TODO() )
# 909 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
              ( TODO() )
# 916 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                             ( _2 )
# 923 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 150 "parser.mly"
                                       ( TODO() )
# 931 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "parser.mly"
 ( [] )
# 937 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 154 "parser.mly"
                ( List.rev _1 )
# 944 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
      ( [_1] )
# 951 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
                           ( _3 :: _1 )
# 959 "parser.ml"
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
