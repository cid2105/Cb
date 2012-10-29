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
\008\000\010\000\010\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\012\000\012\000\012\000\011\000\011\000\011\000\
\011\000\011\000\011\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\013\000\
\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\002\000\000\000\002\000\002\000\003\000\007\000\009\000\006\000\
\008\000\000\000\006\000\003\000\011\000\005\000\011\000\006\000\
\006\000\006\000\000\000\003\000\005\000\001\000\001\000\003\000\
\003\000\003\000\003\000\001\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\003\000\004\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\027\000\027\000\000\000\022\000\000\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\024\000\000\000\025\000\026\000\009\000\010\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\038\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\011\000\
\000\000\030\000\031\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\000\057\000\
\058\000\059\000\060\000\061\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\
\000\000\000\000\000\000\062\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\035\000\021\000\023\000\063\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\000\000\000\000\016\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\017\000\015\000\000\000\
\000\000\019\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\044\000\059\000\045\000\046\000\072\000\
\073\000\157\000\076\000\040\000\118\000\119\000"

let yysindex = "\011\000\
\000\000\000\000\030\255\032\255\036\255\037\255\058\255\077\255\
\087\255\112\255\057\255\000\000\000\000\246\254\071\255\072\255\
\073\255\074\255\075\255\076\255\125\255\000\000\127\255\118\255\
\122\255\121\255\123\255\124\255\133\255\101\255\148\255\153\255\
\000\000\000\000\000\000\156\255\000\000\141\255\000\000\007\255\
\040\255\116\255\190\255\162\255\149\255\000\000\201\255\117\255\
\000\000\198\255\000\000\000\000\000\000\000\000\156\255\152\255\
\154\255\184\255\033\255\000\000\034\255\034\255\228\255\000\000\
\001\255\192\255\193\255\202\255\056\255\056\255\000\000\000\000\
\194\000\000\000\000\000\045\255\099\000\000\000\056\255\056\255\
\233\255\056\255\056\255\156\255\012\001\231\000\056\255\056\255\
\056\255\056\255\056\255\056\255\056\255\056\255\056\255\056\255\
\056\255\056\255\056\255\056\255\056\255\056\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\255\034\255\034\255\
\034\255\215\255\225\255\146\001\108\001\209\255\242\255\000\000\
\036\001\060\001\013\000\000\000\000\000\128\001\128\001\146\001\
\146\001\055\255\146\001\167\000\167\000\055\255\055\255\164\001\
\164\001\182\001\182\001\182\001\182\001\239\254\239\254\000\000\
\000\000\000\000\000\000\000\000\056\255\048\255\048\255\015\000\
\108\001\000\000\236\255\001\000\004\255\000\000\048\255\048\255\
\000\000\003\000\248\255\255\255\056\255\000\000\000\000\084\001\
\048\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\058\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\151\255\000\000\000\000\000\000\000\000\000\000\000\000\
\082\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\112\000\220\254\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\113\255\115\255\118\000\
\137\000\120\255\156\000\216\255\235\255\158\255\197\255\074\000\
\093\000\254\255\017\000\036\000\055\000\173\255\237\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\207\255\040\000\
\187\255\000\000\196\255\226\255\000\000\000\000"

let yytablesize = 731
let yytable = "\085\000\
\086\000\077\000\066\000\041\000\042\000\060\000\112\000\113\000\
\048\000\116\000\117\000\001\000\121\000\122\000\160\000\079\000\
\066\000\126\000\127\000\128\000\129\000\130\000\131\000\132\000\
\133\000\134\000\135\000\136\000\137\000\138\000\139\000\140\000\
\141\000\064\000\123\000\004\000\074\000\075\000\080\000\014\000\
\065\000\022\000\066\000\015\000\016\000\067\000\068\000\049\000\
\064\000\142\000\143\000\144\000\145\000\161\000\081\000\065\000\
\064\000\066\000\162\000\050\000\067\000\068\000\021\000\065\000\
\067\000\017\000\110\000\111\000\112\000\113\000\069\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\067\000\153\000\
\051\000\070\000\071\000\114\000\018\000\069\000\103\000\104\000\
\105\000\106\000\107\000\108\000\050\000\069\000\019\000\168\000\
\070\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\020\000\
\036\000\023\000\024\000\025\000\026\000\027\000\028\000\030\000\
\040\000\040\000\041\000\041\000\029\000\036\000\036\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\049\000\040\000\
\037\000\041\000\038\000\031\000\052\000\057\000\049\000\032\000\
\033\000\043\000\034\000\035\000\040\000\040\000\041\000\041\000\
\050\000\050\000\036\000\049\000\049\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\154\000\155\000\028\000\
\039\000\047\000\032\000\032\000\047\000\053\000\163\000\164\000\
\054\000\055\000\056\000\028\000\061\000\058\000\062\000\063\000\
\170\000\047\000\047\000\032\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\078\000\082\000\083\000\045\000\
\045\000\045\000\045\000\048\000\045\000\045\000\045\000\084\000\
\120\000\045\000\045\000\045\000\045\000\045\000\045\000\148\000\
\048\000\048\000\046\000\046\000\046\000\046\000\045\000\046\000\
\046\000\046\000\033\000\033\000\046\000\046\000\046\000\046\000\
\046\000\046\000\146\000\045\000\045\000\052\000\052\000\052\000\
\052\000\046\000\052\000\033\000\147\000\152\000\156\000\052\000\
\052\000\052\000\052\000\052\000\052\000\158\000\046\000\046\000\
\053\000\053\000\053\000\053\000\052\000\053\000\149\000\159\000\
\165\000\166\000\053\000\053\000\053\000\053\000\053\000\053\000\
\167\000\052\000\052\000\054\000\054\000\054\000\054\000\053\000\
\054\000\068\000\005\000\006\000\064\000\054\000\054\000\054\000\
\054\000\054\000\054\000\065\000\053\000\053\000\055\000\055\000\
\055\000\055\000\054\000\055\000\000\000\000\000\000\000\000\000\
\055\000\055\000\055\000\055\000\055\000\055\000\000\000\054\000\
\054\000\050\000\050\000\050\000\050\000\055\000\050\000\000\000\
\000\000\000\000\000\000\050\000\050\000\000\000\000\000\000\000\
\000\000\000\000\055\000\055\000\051\000\051\000\051\000\051\000\
\050\000\051\000\000\000\000\000\000\000\000\000\051\000\051\000\
\110\000\111\000\112\000\113\000\000\000\050\000\050\000\039\000\
\039\000\039\000\039\000\051\000\039\000\042\000\042\000\042\000\
\042\000\115\000\042\000\000\000\000\000\000\000\000\000\000\000\
\051\000\051\000\000\000\000\000\000\000\000\000\039\000\000\000\
\043\000\043\000\043\000\043\000\042\000\043\000\000\000\000\000\
\000\000\000\000\000\000\039\000\039\000\000\000\000\000\000\000\
\000\000\042\000\042\000\044\000\044\000\044\000\044\000\043\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\091\000\000\000\043\000\043\000\095\000\096\000\
\000\000\000\000\044\000\000\000\000\000\000\000\103\000\104\000\
\105\000\106\000\107\000\108\000\000\000\000\000\000\000\044\000\
\044\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\109\000\087\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\098\000\099\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\108\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\125\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\000\000\124\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\000\000\150\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\000\000\151\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\000\000\169\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\091\000\000\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\091\000\
\000\000\093\000\094\000\095\000\096\000\000\000\000\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\091\000\000\000\093\000\094\000\095\000\096\000\000\000\
\000\000\000\000\000\000\000\000\000\000\103\000\104\000\105\000\
\106\000\107\000\108\000"

let yycheck = "\069\000\
\070\000\062\000\039\001\034\000\035\000\055\000\024\001\025\001\
\039\000\079\000\080\000\001\000\082\000\083\000\011\001\015\001\
\053\001\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\001\001\084\000\006\001\003\001\004\001\038\001\008\001\
\008\001\052\001\010\001\008\001\008\001\013\001\014\001\041\001\
\001\001\110\000\111\000\112\000\113\000\050\001\054\001\008\001\
\001\001\010\001\055\001\053\001\013\001\014\001\006\001\008\001\
\039\001\008\001\022\001\023\001\024\001\025\001\038\001\042\001\
\043\001\044\001\045\001\046\001\047\001\048\001\053\001\149\000\
\041\001\049\001\050\001\039\001\008\001\038\001\032\001\033\001\
\034\001\035\001\036\001\037\001\053\001\038\001\008\001\165\000\
\049\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\008\001\
\039\001\051\001\051\001\051\001\051\001\051\001\051\001\001\001\
\016\001\017\001\016\001\017\001\008\001\052\001\053\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\039\001\
\052\001\039\001\007\001\038\001\041\001\041\001\039\001\038\001\
\040\001\006\001\040\001\040\001\052\001\053\001\052\001\053\001\
\053\001\053\001\038\001\052\001\053\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\150\000\151\000\041\001\
\040\001\053\001\022\001\023\001\039\001\008\001\159\000\160\000\
\039\001\053\001\002\001\053\001\053\001\008\001\053\001\024\001\
\169\000\052\001\053\001\039\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\001\001\038\001\038\001\016\001\
\017\001\018\001\019\001\039\001\021\001\022\001\023\001\038\001\
\008\001\026\001\027\001\028\001\029\001\030\001\031\001\039\001\
\052\001\053\001\016\001\017\001\018\001\019\001\039\001\021\001\
\022\001\023\001\022\001\023\001\026\001\027\001\028\001\029\001\
\030\001\031\001\052\001\052\001\053\001\016\001\017\001\018\001\
\019\001\039\001\021\001\039\001\052\001\009\001\008\001\026\001\
\027\001\028\001\029\001\030\001\031\001\050\001\052\001\053\001\
\016\001\017\001\018\001\019\001\039\001\021\001\053\001\039\001\
\038\001\050\001\026\001\027\001\028\001\029\001\030\001\031\001\
\050\001\052\001\053\001\016\001\017\001\018\001\019\001\039\001\
\021\001\000\000\039\001\039\001\039\001\026\001\027\001\028\001\
\029\001\030\001\031\001\039\001\052\001\053\001\016\001\017\001\
\018\001\019\001\039\001\021\001\255\255\255\255\255\255\255\255\
\026\001\027\001\028\001\029\001\030\001\031\001\255\255\052\001\
\053\001\016\001\017\001\018\001\019\001\039\001\021\001\255\255\
\255\255\255\255\255\255\026\001\027\001\255\255\255\255\255\255\
\255\255\255\255\052\001\053\001\016\001\017\001\018\001\019\001\
\039\001\021\001\255\255\255\255\255\255\255\255\026\001\027\001\
\022\001\023\001\024\001\025\001\255\255\052\001\053\001\016\001\
\017\001\018\001\019\001\039\001\021\001\016\001\017\001\018\001\
\019\001\039\001\021\001\255\255\255\255\255\255\255\255\255\255\
\052\001\053\001\255\255\255\255\255\255\255\255\039\001\255\255\
\016\001\017\001\018\001\019\001\039\001\021\001\255\255\255\255\
\255\255\255\255\255\255\052\001\053\001\255\255\255\255\255\255\
\255\255\052\001\053\001\016\001\017\001\018\001\019\001\039\001\
\021\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\020\001\255\255\052\001\053\001\024\001\025\001\
\255\255\255\255\039\001\255\255\255\255\255\255\032\001\033\001\
\034\001\035\001\036\001\037\001\255\255\255\255\255\255\052\001\
\053\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\052\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\052\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\039\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\039\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\039\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\039\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\020\001\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\020\001\
\255\255\022\001\023\001\024\001\025\001\255\255\255\255\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\020\001\255\255\022\001\023\001\024\001\025\001\255\255\
\255\255\255\255\255\255\255\255\255\255\032\001\033\001\034\001\
\035\001\036\001\037\001"

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
# 71 "parser.mly"
( [], [] )
# 479 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 72 "parser.mly"
                ( (_2 :: fst _1), snd _1 )
# 487 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methdecl) in
    Obj.repr(
# 73 "parser.mly"
                   ( TODO() )
# 495 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'meth_params) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 76 "parser.mly"
                                                                      ( create() )
# 505 "parser.ml"
               : 'methdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
 ( [] )
# 511 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 80 "parser.mly"
              ( List.rev(_1) )
# 518 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 83 "parser.mly"
            ( [_1] )
# 525 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 84 "parser.mly"
                               ( _3 :: _1 )
# 533 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
             ( TODO() )
# 541 "parser.ml"
               : 'param_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
 ( [] )
# 547 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 91 "parser.mly"
                            ( _2 :: _1 )
# 555 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                ( TODO() )
# 562 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                         ( Return(_2) )
# 569 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'elsif_statement) in
    Obj.repr(
# 96 "parser.mly"
                                                                           ( TODO() )
# 578 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'statement) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'elsif_statement) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 97 "parser.mly"
                                                                             ( TODO() )
# 588 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 98 "parser.mly"
                                                 ( TODO() )
# 596 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'param_decl) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 99 "parser.mly"
                                                               (TODO())
# 605 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
                    ( [] )
# 611 "parser.ml"
               : 'elsif_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'elsif_statement) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 103 "parser.mly"
                                                             ( TODO() )
# 620 "parser.ml"
               : 'elsif_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 106 "parser.mly"
                       ({ vartype = _1; varname = _2})
# 628 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 107 "parser.mly"
                                                                                             ( TODO() )
# 638 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 108 "parser.mly"
                                       ( create(_2) )
# 646 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'generic_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 109 "parser.mly"
                                                                                                 ( TODO() )
# 655 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 110 "parser.mly"
                                             ( TODO() )
# 663 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 111 "parser.mly"
                                              ( TODO() )
# 671 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 112 "parser.mly"
                                             ( TODO() )
# 679 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
 ( [%1] )
# 685 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                         ( _3 :: _1 )
# 693 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 117 "parser.mly"
                                          ( TODO() )
# 702 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "parser.mly"
             ( _1 )
# 709 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                 ( _1 )
# 716 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 122 "parser.mly"
                                    ( Binop(_1, Add, _3)  )
# 724 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 123 "parser.mly"
                                     ( Binop(_1, Sub, _3)  )
# 732 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 124 "parser.mly"
                                     ( Binop(_1, Mult, _3)  )
# 740 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 125 "parser.mly"
                                      ( Binop(_1, Div, _3)  )
# 748 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "parser.mly"
    ( Id(_1) )
# 755 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
             ( TODO() )
# 763 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 130 "parser.mly"
              ( TODO() )
# 770 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                  ( TODO() )
# 778 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                    ( Assign(_1, BinOp(_1, Add, _3)) )
# 786 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                     ( Assign(_1, BinOp(_1, Sub, _3)) )
# 794 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                     ( Assign(_1, BinOp(_1, Mult, _3)) )
# 802 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                      ( Assign(_1, BinOp(_1, Div, _3)) )
# 810 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                   ( Assign(_1, BinOp(_1, Mod, _3)) )
# 818 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                  ( BinOp(_1, Add, _3) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                   ( BinOp(_1, Sub, _3) )
# 834 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                   ( BinOp(_1, Mult, _3) )
# 842 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                    ( BinOp(_1, Div, _3) )
# 850 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                 ( BinOp(_1, Mod, _3) )
# 858 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                ( BinOp(_1, Eq, _3) )
# 866 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                  ( BinOp(_1, NEq, _3) )
# 874 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                ( BinOp(_1, Less, _3) )
# 882 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                 ( BinOp(_1, LEq, _3) )
# 890 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                ( BinOp(_1, Greater, _3) )
# 898 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                 ( BinOp(_1, GEq, _3) )
# 906 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                 ( Assign(_1, BinOp(_1, Add, IntLiteral(1))) )
# 913 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                   ( Assign(_1, BinOp(_1, Sub, IntLiteral(1))) )
# 920 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
              ( TODO() )
# 927 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
             ( TODO() )
# 934 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
              ( TODO() )
# 941 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
              ( TODO() )
# 948 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                             ( _2 )
# 955 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 155 "parser.mly"
                                       ( TODO() )
# 963 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "parser.mly"
 ( [] )
# 969 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 159 "parser.mly"
                ( List.rev _1 )
# 976 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
      ( [_1] )
# 983 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parser.mly"
                           ( _3 :: _1 )
# 991 "parser.ml"
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
