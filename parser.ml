type token =
  | INTLITERAL of (int)
  | OCTAVE of (int)
  | DURATIONINT of (int)
  | DURATIONCONST of (string)
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
# 60 "parser.ml"
let yytransl_const = [|
  264 (* IN *);
  265 (* IF *);
  266 (* ELSE *);
  267 (* NOELSE *);
  268 (* WHILE *);
  269 (* FOREACH *);
  270 (* ASSIGN *);
  271 (* PLUSEQ *);
  272 (* MINUSEQ *);
  273 (* TIMESEQ *);
  274 (* DIVIDEEQ *);
  275 (* MOD *);
  276 (* MODEQ *);
  277 (* PLUS *);
  278 (* MINUS *);
  279 (* TIMES *);
  280 (* DIVIDE *);
  281 (* IS *);
  282 (* ISNT *);
  283 (* LT *);
  284 (* LEQ *);
  285 (* GT *);
  286 (* GEQ *);
  287 (* PLUSPLUS *);
  288 (* MINUSMINUS *);
  289 (* SHARP *);
  290 (* FLAT *);
  291 (* RAISE *);
  292 (* LOWER *);
  293 (* LEFTPAREN *);
  294 (* RIGHTPAREN *);
  295 (* LBRAC *);
  296 (* RBRAC *);
  297 (* INT *);
  298 (* NOTE *);
  299 (* CHORD *);
  300 (* SCALE *);
  301 (* STANZA *);
  302 (* SCORE *);
  303 (* METH *);
  304 (* RETURN *);
  305 (* END *);
  306 (* VASSIGN *);
  307 (* SEMICOLON *);
  308 (* COMMA *);
  309 (* DOT *);
    0|]

let yytransl_block = [|
  257 (* INTLITERAL *);
  258 (* OCTAVE *);
  259 (* DURATIONINT *);
  260 (* DURATIONCONST *);
  261 (* DATATYPE *);
  262 (* NOTECONST *);
  263 (* ID *);
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

let yysindex = "\003\000\
\000\000\000\000\030\255\255\254\008\255\034\255\036\255\051\255\
\057\255\084\255\007\255\000\000\000\000\252\254\042\255\044\255\
\069\255\073\255\074\255\075\255\125\255\000\000\126\255\118\255\
\122\255\121\255\123\255\124\255\133\255\101\255\148\255\161\255\
\000\000\000\000\000\000\156\255\000\000\145\255\000\000\116\255\
\117\255\151\255\194\255\164\255\154\255\000\000\205\255\152\255\
\000\000\221\255\000\000\000\000\000\000\000\000\156\255\177\255\
\178\255\217\255\033\255\000\000\005\255\005\255\238\255\000\000\
\002\255\210\255\222\255\229\255\056\255\056\255\000\000\000\000\
\194\000\000\000\000\000\045\255\099\000\000\000\056\255\056\255\
\251\255\056\255\056\255\156\255\012\001\231\000\056\255\056\255\
\056\255\056\255\056\255\056\255\056\255\056\255\056\255\056\255\
\056\255\056\255\056\255\056\255\056\255\056\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\255\005\255\005\255\
\005\255\224\255\225\255\146\001\108\001\239\255\226\255\000\000\
\036\001\060\001\021\000\000\000\000\000\128\001\128\001\146\001\
\146\001\054\255\146\001\167\000\167\000\054\255\054\255\164\001\
\164\001\182\001\182\001\182\001\182\001\014\255\014\255\000\000\
\000\000\000\000\000\000\000\000\056\255\047\255\047\255\031\000\
\108\001\000\000\246\255\002\000\251\254\000\000\047\255\047\255\
\000\000\004\000\255\255\008\000\056\255\000\000\000\000\084\001\
\047\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\058\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\153\255\000\000\000\000\000\000\000\000\000\000\000\000\
\082\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\112\000\221\254\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\113\255\115\255\118\000\
\137\000\120\255\156\000\216\255\235\255\158\255\197\255\074\000\
\093\000\254\255\017\000\036\000\055\000\040\255\173\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\208\255\039\000\
\187\255\000\000\196\255\087\000\000\000\000\000"

let yytablesize = 730
let yytable = "\085\000\
\086\000\077\000\066\000\001\000\160\000\014\000\060\000\074\000\
\075\000\116\000\117\000\021\000\121\000\122\000\015\000\079\000\
\066\000\126\000\127\000\128\000\129\000\130\000\131\000\132\000\
\133\000\134\000\135\000\136\000\137\000\138\000\139\000\140\000\
\141\000\064\000\004\000\123\000\112\000\113\000\080\000\065\000\
\016\000\066\000\017\000\161\000\067\000\068\000\022\000\064\000\
\162\000\142\000\143\000\144\000\145\000\065\000\081\000\066\000\
\064\000\018\000\067\000\068\000\032\000\032\000\065\000\019\000\
\067\000\110\000\111\000\112\000\113\000\069\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\032\000\067\000\153\000\
\070\000\071\000\114\000\069\000\103\000\104\000\105\000\106\000\
\107\000\108\000\020\000\023\000\069\000\024\000\070\000\168\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\025\000\036\000\
\041\000\042\000\026\000\027\000\028\000\048\000\030\000\040\000\
\040\000\041\000\041\000\029\000\036\000\036\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\040\000\037\000\
\041\000\038\000\031\000\049\000\051\000\049\000\032\000\033\000\
\043\000\034\000\035\000\040\000\040\000\041\000\041\000\050\000\
\050\000\036\000\049\000\049\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\154\000\155\000\052\000\057\000\
\028\000\033\000\033\000\047\000\047\000\163\000\164\000\039\000\
\053\000\054\000\050\000\050\000\028\000\055\000\056\000\170\000\
\047\000\047\000\033\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\058\000\061\000\062\000\045\000\045\000\
\045\000\045\000\048\000\045\000\045\000\045\000\078\000\063\000\
\045\000\045\000\045\000\045\000\045\000\045\000\082\000\048\000\
\048\000\046\000\046\000\046\000\046\000\045\000\046\000\046\000\
\046\000\120\000\083\000\046\000\046\000\046\000\046\000\046\000\
\046\000\084\000\045\000\045\000\052\000\052\000\052\000\052\000\
\046\000\052\000\146\000\147\000\148\000\149\000\052\000\052\000\
\052\000\052\000\052\000\052\000\152\000\046\000\046\000\053\000\
\053\000\053\000\053\000\052\000\053\000\156\000\158\000\159\000\
\165\000\053\000\053\000\053\000\053\000\053\000\053\000\166\000\
\052\000\052\000\054\000\054\000\054\000\054\000\053\000\054\000\
\167\000\068\000\000\000\005\000\054\000\054\000\054\000\054\000\
\054\000\054\000\006\000\053\000\053\000\055\000\055\000\055\000\
\055\000\054\000\055\000\064\000\000\000\065\000\000\000\055\000\
\055\000\055\000\055\000\055\000\055\000\000\000\054\000\054\000\
\050\000\050\000\050\000\050\000\055\000\050\000\000\000\000\000\
\000\000\000\000\050\000\050\000\000\000\000\000\000\000\000\000\
\000\000\055\000\055\000\051\000\051\000\051\000\051\000\050\000\
\051\000\000\000\000\000\000\000\000\000\051\000\051\000\110\000\
\111\000\112\000\113\000\000\000\050\000\050\000\039\000\039\000\
\039\000\039\000\051\000\039\000\042\000\042\000\042\000\042\000\
\115\000\042\000\000\000\000\000\000\000\000\000\000\000\051\000\
\051\000\000\000\000\000\000\000\000\000\039\000\000\000\043\000\
\043\000\043\000\043\000\042\000\043\000\000\000\000\000\000\000\
\000\000\000\000\039\000\039\000\000\000\000\000\000\000\000\000\
\042\000\042\000\044\000\044\000\044\000\044\000\043\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\091\000\000\000\043\000\043\000\095\000\096\000\000\000\
\000\000\044\000\000\000\000\000\000\000\103\000\104\000\105\000\
\106\000\107\000\108\000\000\000\000\000\000\000\044\000\044\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\108\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\109\000\087\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\096\000\097\000\
\098\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\107\000\108\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\125\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\000\000\124\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\000\000\150\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\000\000\151\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\000\000\169\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\098\000\099\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\108\000\091\000\000\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\108\000\091\000\000\000\
\093\000\094\000\095\000\096\000\000\000\000\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\091\000\000\000\093\000\094\000\095\000\096\000\000\000\000\000\
\000\000\000\000\000\000\000\000\103\000\104\000\105\000\106\000\
\107\000\108\000"

let yycheck = "\069\000\
\070\000\062\000\038\001\001\000\010\001\007\001\055\000\003\001\
\004\001\079\000\080\000\005\001\082\000\083\000\007\001\014\001\
\052\001\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\001\001\005\001\084\000\023\001\024\001\037\001\007\001\
\007\001\009\001\007\001\049\001\012\001\013\001\051\001\001\001\
\054\001\110\000\111\000\112\000\113\000\007\001\053\001\009\001\
\001\001\007\001\012\001\013\001\021\001\022\001\007\001\007\001\
\038\001\021\001\022\001\023\001\024\001\037\001\041\001\042\001\
\043\001\044\001\045\001\046\001\047\001\038\001\052\001\149\000\
\048\001\049\001\038\001\037\001\031\001\032\001\033\001\034\001\
\035\001\036\001\007\001\050\001\037\001\050\001\048\001\165\000\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\050\001\038\001\
\034\000\035\000\050\001\050\001\050\001\039\000\001\001\015\001\
\016\001\015\001\016\001\007\001\051\001\052\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\038\001\051\001\
\038\001\006\001\037\001\040\001\040\001\038\001\037\001\039\001\
\005\001\039\001\039\001\051\001\052\001\051\001\052\001\052\001\
\052\001\037\001\051\001\052\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\150\000\151\000\040\001\040\001\
\040\001\021\001\022\001\038\001\052\001\159\000\160\000\039\001\
\007\001\038\001\052\001\052\001\052\001\052\001\002\001\169\000\
\051\001\052\001\038\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\007\001\052\001\052\001\015\001\016\001\
\017\001\018\001\038\001\020\001\021\001\022\001\001\001\023\001\
\025\001\026\001\027\001\028\001\029\001\030\001\037\001\051\001\
\052\001\015\001\016\001\017\001\018\001\038\001\020\001\021\001\
\022\001\007\001\037\001\025\001\026\001\027\001\028\001\029\001\
\030\001\037\001\051\001\052\001\015\001\016\001\017\001\018\001\
\038\001\020\001\051\001\051\001\038\001\052\001\025\001\026\001\
\027\001\028\001\029\001\030\001\008\001\051\001\052\001\015\001\
\016\001\017\001\018\001\038\001\020\001\007\001\049\001\038\001\
\037\001\025\001\026\001\027\001\028\001\029\001\030\001\049\001\
\051\001\052\001\015\001\016\001\017\001\018\001\038\001\020\001\
\049\001\000\000\255\255\038\001\025\001\026\001\027\001\028\001\
\029\001\030\001\038\001\051\001\052\001\015\001\016\001\017\001\
\018\001\038\001\020\001\038\001\255\255\038\001\255\255\025\001\
\026\001\027\001\028\001\029\001\030\001\255\255\051\001\052\001\
\015\001\016\001\017\001\018\001\038\001\020\001\255\255\255\255\
\255\255\255\255\025\001\026\001\255\255\255\255\255\255\255\255\
\255\255\051\001\052\001\015\001\016\001\017\001\018\001\038\001\
\020\001\255\255\255\255\255\255\255\255\025\001\026\001\021\001\
\022\001\023\001\024\001\255\255\051\001\052\001\015\001\016\001\
\017\001\018\001\038\001\020\001\015\001\016\001\017\001\018\001\
\038\001\020\001\255\255\255\255\255\255\255\255\255\255\051\001\
\052\001\255\255\255\255\255\255\255\255\038\001\255\255\015\001\
\016\001\017\001\018\001\038\001\020\001\255\255\255\255\255\255\
\255\255\255\255\051\001\052\001\255\255\255\255\255\255\255\255\
\051\001\052\001\015\001\016\001\017\001\018\001\038\001\020\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\019\001\255\255\051\001\052\001\023\001\024\001\255\255\
\255\255\038\001\255\255\255\255\255\255\031\001\032\001\033\001\
\034\001\035\001\036\001\255\255\255\255\255\255\051\001\052\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\051\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\051\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\255\255\038\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\255\255\038\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\255\255\038\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\255\255\038\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\019\001\255\255\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\019\001\255\255\
\021\001\022\001\023\001\024\001\255\255\255\255\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\019\001\255\255\021\001\022\001\023\001\024\001\255\255\255\255\
\255\255\255\255\255\255\255\255\031\001\032\001\033\001\034\001\
\035\001\036\001"

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
# 476 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 71 "parser.mly"
                ( (_2 :: fst _1), snd _1 )
# 484 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methdecl) in
    Obj.repr(
# 72 "parser.mly"
                   ( TODO() )
# 492 "parser.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'meth_params) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 75 "parser.mly"
                                                                      ( create() )
# 502 "parser.ml"
               : 'methdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
 ( [] )
# 508 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 79 "parser.mly"
              ( List.rev(_1) )
# 515 "parser.ml"
               : 'meth_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 82 "parser.mly"
            ( [_1] )
# 522 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_decl) in
    Obj.repr(
# 83 "parser.mly"
                               ( _3 :: _1 )
# 530 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
             ( TODO() )
# 538 "parser.ml"
               : 'param_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
 ( [] )
# 544 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 90 "parser.mly"
                            ( _2 :: _1 )
# 552 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                ( TODO() )
# 559 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                         ( Return(_2) )
# 566 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'elsif_statement) in
    Obj.repr(
# 95 "parser.mly"
                                                                           ( TODO() )
# 575 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'statement) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'elsif_statement) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 96 "parser.mly"
                                                                             ( TODO() )
# 585 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 97 "parser.mly"
                                                 ( TODO() )
# 593 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'param_decl) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 98 "parser.mly"
                                                               (TODO())
# 602 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                    ( [] )
# 608 "parser.ml"
               : 'elsif_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'elsif_statement) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 102 "parser.mly"
                                                             ( TODO() )
# 617 "parser.ml"
               : 'elsif_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "parser.mly"
                       ({ vartype = _1; varname = _2})
# 625 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 106 "parser.mly"
                                                                                             ( TODO() )
# 635 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 107 "parser.mly"
                                       ( create(_2) )
# 643 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'generic_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    Obj.repr(
# 108 "parser.mly"
                                                                                                 ( TODO() )
# 652 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 109 "parser.mly"
                                             ( TODO() )
# 660 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 110 "parser.mly"
                                              ( TODO() )
# 668 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'generic_list) in
    Obj.repr(
# 111 "parser.mly"
                                             ( TODO() )
# 676 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
 ( [%1] )
# 682 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
                         ( _3 :: _1 )
# 690 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'generic_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 116 "parser.mly"
                                          ( TODO() )
# 699 "parser.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 119 "parser.mly"
             ( _1 )
# 706 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
                 ( _1 )
# 713 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 121 "parser.mly"
                                    ( Binop(_1, Add, _3)  )
# 721 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 122 "parser.mly"
                                     ( Binop(_1, Sub, _3)  )
# 729 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 123 "parser.mly"
                                     ( Binop(_1, Mult, _3)  )
# 737 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 124 "parser.mly"
                                      ( Binop(_1, Div, _3)  )
# 745 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 127 "parser.mly"
    ( Id(_1) )
# 752 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "parser.mly"
             ( TODO() )
# 760 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "parser.mly"
              ( TODO() )
# 767 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                  ( TODO() )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                    ( Assign(_1, BinOp(_1, Add, _3)) )
# 783 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                     ( Assign(_1, BinOp(_1, Sub, _3)) )
# 791 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                     ( Assign(_1, BinOp(_1, Mult, _3)) )
# 799 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                      ( Assign(_1, BinOp(_1, Div, _3)) )
# 807 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                   ( Assign(_1, BinOp(_1, Mod, _3)) )
# 815 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                  ( BinOp(_1, Add, _3) )
# 823 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                   ( BinOp(_1, Sub, _3) )
# 831 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                   ( BinOp(_1, Mult, _3) )
# 839 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                    ( BinOp(_1, Div, _3) )
# 847 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                 ( BinOp(_1, Mod, _3) )
# 855 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                ( BinOp(_1, Eq, _3) )
# 863 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                  ( BinOp(_1, NEq, _3) )
# 871 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                ( BinOp(_1, Less, _3) )
# 879 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                 ( BinOp(_1, LEq, _3) )
# 887 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                ( BinOp(_1, Greater, _3) )
# 895 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                 ( BinOp(_1, GEq, _3) )
# 903 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                 ( Assign(_1, BinOp(_1, Add, IntLiteral(1))) )
# 910 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                   ( Assign(_1, BinOp(_1, Sub, IntLiteral(1))) )
# 917 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
              ( TODO() )
# 924 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
             ( TODO() )
# 931 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
              ( TODO() )
# 938 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
              ( TODO() )
# 945 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                             ( _2 )
# 952 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 154 "parser.mly"
                                       ( TODO() )
# 960 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
 ( [] )
# 966 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 158 "parser.mly"
                ( List.rev _1 )
# 973 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
      ( [_1] )
# 980 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
                           ( _3 :: _1 )
# 988 "parser.ml"
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
