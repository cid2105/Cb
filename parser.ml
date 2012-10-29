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
\011\000\011\000\010\000\010\000\010\000\010\000\010\000\010\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\012\000\012\000\013\000\013\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\002\000\000\000\002\000\002\000\003\000\006\000\008\000\006\000\
\008\000\003\000\011\000\005\000\011\000\006\000\006\000\006\000\
\000\000\003\000\001\000\001\000\003\000\003\000\003\000\003\000\
\001\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\004\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\025\000\025\000\000\000\020\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\022\000\000\000\023\000\024\000\009\000\010\000\000\000\000\000\
\000\000\026\000\000\000\008\000\000\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\011\000\000\000\
\027\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\053\000\054\000\055\000\056\000\
\057\000\058\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\059\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\032\000\019\000\
\021\000\060\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\014\000\016\000\000\000\000\000\000\000\
\015\000\017\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\044\000\059\000\045\000\046\000\071\000\
\072\000\075\000\040\000\116\000\117\000"

let yysindex = "\007\000\
\000\000\000\000\029\255\003\255\008\255\028\255\033\255\036\255\
\055\255\057\255\085\255\000\000\000\000\000\255\010\255\071\255\
\072\255\073\255\075\255\078\255\123\255\000\000\155\255\120\255\
\121\255\122\255\125\255\126\255\151\255\108\255\154\255\127\255\
\000\000\000\000\000\000\201\255\000\000\174\255\000\000\218\254\
\077\255\153\255\220\255\198\255\192\255\000\000\006\000\185\255\
\000\000\010\000\000\000\000\000\000\000\000\000\201\255\230\255\
\240\255\000\000\032\255\000\000\002\255\002\255\000\000\255\254\
\001\000\008\000\017\000\054\255\054\255\000\000\000\000\191\000\
\000\000\000\000\044\255\129\255\054\255\054\255\030\000\054\255\
\054\255\201\255\009\001\228\000\054\255\054\255\054\255\054\255\
\054\255\054\255\054\255\054\255\054\255\054\255\054\255\054\255\
\054\255\054\255\054\255\054\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\255\002\255\002\255\002\255\004\000\
\005\000\119\001\081\001\019\000\012\000\000\000\033\001\057\001\
\065\000\000\000\000\000\101\001\101\001\119\001\119\001\053\255\
\119\001\164\000\164\000\053\255\053\255\168\255\168\255\137\001\
\137\001\137\001\137\001\014\255\014\255\000\000\000\000\000\000\
\000\000\000\000\054\255\046\255\046\255\067\000\081\001\249\254\
\026\000\038\000\046\255\000\000\000\000\046\255\034\000\045\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\096\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\062\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\080\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\063\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\109\000\025\255\000\000\073\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\255\111\255\115\000\134\000\118\255\
\153\000\213\255\232\255\156\255\194\255\071\000\090\000\251\255\
\014\000\033\000\052\000\234\255\253\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\116\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\208\255\201\000\
\188\255\196\255\086\000\000\000\000\000"

let yytablesize = 686
let yytable = "\083\000\
\084\000\076\000\049\000\155\000\073\000\074\000\060\000\001\000\
\114\000\115\000\014\000\119\000\120\000\077\000\050\000\015\000\
\124\000\125\000\126\000\127\000\128\000\129\000\130\000\131\000\
\132\000\133\000\134\000\135\000\136\000\137\000\138\000\139\000\
\063\000\121\000\004\000\016\000\078\000\110\000\111\000\064\000\
\017\000\065\000\156\000\018\000\066\000\067\000\063\000\140\000\
\141\000\142\000\143\000\022\000\079\000\064\000\063\000\065\000\
\037\000\037\000\066\000\067\000\023\000\064\000\019\000\063\000\
\020\000\108\000\109\000\110\000\111\000\068\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\063\000\151\000\037\000\
\069\000\070\000\112\000\068\000\101\000\102\000\103\000\104\000\
\105\000\106\000\021\000\068\000\037\000\037\000\069\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\051\000\033\000\041\000\
\042\000\024\000\025\000\026\000\048\000\027\000\038\000\038\000\
\028\000\050\000\029\000\033\000\033\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\038\000\108\000\109\000\
\110\000\111\000\064\000\030\000\046\000\031\000\032\000\037\000\
\038\000\033\000\038\000\038\000\034\000\035\000\039\000\113\000\
\064\000\046\000\046\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\089\000\036\000\091\000\092\000\093\000\
\094\000\052\000\044\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\050\000\043\000\044\000\
\044\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\057\000\047\000\053\000\042\000\042\000\042\000\042\000\
\045\000\042\000\042\000\042\000\054\000\050\000\042\000\042\000\
\042\000\042\000\042\000\042\000\055\000\045\000\045\000\043\000\
\043\000\043\000\043\000\042\000\043\000\043\000\043\000\029\000\
\029\000\043\000\043\000\043\000\043\000\043\000\043\000\056\000\
\042\000\042\000\049\000\049\000\049\000\049\000\043\000\049\000\
\029\000\058\000\030\000\030\000\049\000\049\000\049\000\049\000\
\049\000\049\000\061\000\043\000\043\000\050\000\050\000\050\000\
\050\000\049\000\050\000\030\000\062\000\118\000\080\000\050\000\
\050\000\050\000\050\000\050\000\050\000\081\000\049\000\049\000\
\051\000\051\000\051\000\051\000\050\000\051\000\082\000\144\000\
\145\000\146\000\051\000\051\000\051\000\051\000\051\000\051\000\
\147\000\050\000\050\000\052\000\052\000\052\000\052\000\051\000\
\052\000\150\000\154\000\157\000\158\000\052\000\052\000\052\000\
\052\000\052\000\052\000\161\000\051\000\051\000\047\000\047\000\
\047\000\047\000\052\000\047\000\152\000\153\000\162\000\065\000\
\047\000\047\000\005\000\159\000\006\000\061\000\160\000\052\000\
\052\000\048\000\048\000\048\000\048\000\047\000\048\000\062\000\
\000\000\000\000\000\000\048\000\048\000\000\000\000\000\000\000\
\000\000\000\000\047\000\047\000\036\000\036\000\036\000\036\000\
\048\000\036\000\039\000\039\000\039\000\039\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\048\000\048\000\000\000\
\000\000\000\000\000\000\036\000\000\000\040\000\040\000\040\000\
\040\000\039\000\040\000\000\000\000\000\000\000\000\000\000\000\
\036\000\036\000\000\000\000\000\000\000\000\000\039\000\039\000\
\041\000\041\000\041\000\041\000\040\000\041\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\089\000\
\000\000\040\000\040\000\093\000\094\000\000\000\000\000\041\000\
\000\000\000\000\000\000\101\000\102\000\103\000\104\000\105\000\
\106\000\000\000\000\000\000\000\041\000\041\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\107\000\085\000\086\000\087\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\096\000\097\000\
\098\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\123\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\000\000\122\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\000\000\148\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\000\000\149\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\087\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\098\000\099\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\089\000\000\000\091\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\089\000\000\000\091\000\092\000\
\093\000\094\000\000\000\000\000\000\000\000\000\000\000\000\000\
\101\000\102\000\103\000\104\000\105\000\106\000"

let yycheck = "\068\000\
\069\000\062\000\041\001\011\001\003\001\004\001\055\000\001\000\
\077\000\078\000\008\001\080\000\081\000\015\001\053\001\008\001\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\001\001\082\000\006\001\008\001\038\001\024\001\025\001\008\001\
\008\001\010\001\050\001\008\001\013\001\014\001\001\001\108\000\
\109\000\110\000\111\000\052\001\054\001\008\001\001\001\010\001\
\016\001\017\001\013\001\014\001\051\001\008\001\008\001\039\001\
\008\001\022\001\023\001\024\001\025\001\038\001\042\001\043\001\
\044\001\045\001\046\001\047\001\048\001\053\001\147\000\039\001\
\049\001\050\001\039\001\038\001\032\001\033\001\034\001\035\001\
\036\001\037\001\006\001\038\001\052\001\053\001\049\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\041\001\039\001\034\000\
\035\000\051\001\051\001\051\001\039\000\051\001\016\001\017\001\
\051\001\053\001\008\001\052\001\053\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\039\001\022\001\023\001\
\024\001\025\001\039\001\001\001\039\001\038\001\038\001\052\001\
\007\001\040\001\052\001\053\001\040\001\040\001\040\001\039\001\
\053\001\052\001\053\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\020\001\038\001\022\001\023\001\024\001\
\025\001\041\001\039\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\053\001\006\001\052\001\
\053\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\041\001\053\001\008\001\016\001\017\001\018\001\019\001\
\039\001\021\001\022\001\023\001\039\001\053\001\026\001\027\001\
\028\001\029\001\030\001\031\001\053\001\052\001\053\001\016\001\
\017\001\018\001\019\001\039\001\021\001\022\001\023\001\022\001\
\023\001\026\001\027\001\028\001\029\001\030\001\031\001\002\001\
\052\001\053\001\016\001\017\001\018\001\019\001\039\001\021\001\
\039\001\008\001\022\001\023\001\026\001\027\001\028\001\029\001\
\030\001\031\001\053\001\052\001\053\001\016\001\017\001\018\001\
\019\001\039\001\021\001\039\001\053\001\008\001\038\001\026\001\
\027\001\028\001\029\001\030\001\031\001\038\001\052\001\053\001\
\016\001\017\001\018\001\019\001\039\001\021\001\038\001\052\001\
\052\001\039\001\026\001\027\001\028\001\029\001\030\001\031\001\
\053\001\052\001\053\001\016\001\017\001\018\001\019\001\039\001\
\021\001\009\001\008\001\050\001\039\001\026\001\027\001\028\001\
\029\001\030\001\031\001\050\001\052\001\053\001\016\001\017\001\
\018\001\019\001\039\001\021\001\148\000\149\000\050\001\000\000\
\026\001\027\001\039\001\155\000\039\001\039\001\158\000\052\001\
\053\001\016\001\017\001\018\001\019\001\039\001\021\001\039\001\
\255\255\255\255\255\255\026\001\027\001\255\255\255\255\255\255\
\255\255\255\255\052\001\053\001\016\001\017\001\018\001\019\001\
\039\001\021\001\016\001\017\001\018\001\019\001\255\255\021\001\
\255\255\255\255\255\255\255\255\255\255\052\001\053\001\255\255\
\255\255\255\255\255\255\039\001\255\255\016\001\017\001\018\001\
\019\001\039\001\021\001\255\255\255\255\255\255\255\255\255\255\
\052\001\053\001\255\255\255\255\255\255\255\255\052\001\053\001\
\016\001\017\001\018\001\019\001\039\001\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\020\001\
\255\255\052\001\053\001\024\001\025\001\255\255\255\255\039\001\
\255\255\255\255\255\255\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\255\255\255\255\052\001\053\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\052\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\052\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\255\255\039\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\255\255\039\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\255\255\039\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\020\001\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\020\001\255\255\022\001\023\001\
\024\001\025\001\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001\033\001\034\001\035\001\036\001\037\001"

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
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 114 "parser.mly"
             ( _1 )
# 668 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
                 ( _1 )
# 675 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 116 "parser.mly"
                                    ( Binop(_1, Add, _3)  )
# 683 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 117 "parser.mly"
                                     ( Binop(_1, Sub, _3)  )
# 691 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 118 "parser.mly"
                                     ( Binop(_1, Mult, _3)  )
# 699 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration_expr) in
    Obj.repr(
# 119 "parser.mly"
                                      ( Binop(_1, Div, _3)  )
# 707 "parser.ml"
               : 'duration_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
    ( Id(_1) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
             ( TODO() )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 124 "parser.mly"
              ( TODO() )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                  ( TODO() )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                    ( Assign(_1, BinOp(_1, Add, _3)) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                     ( Assign(_1, BinOp(_1, Sub, _3)) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( Assign(_1, BinOp(_1, Mult, _3)) )
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                      ( Assign(_1, BinOp(_1, Div, _3)) )
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                   ( Assign(_1, BinOp(_1, Mod, _3)) )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                  ( BinOp(_1, Add, _3) )
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                   ( BinOp(_1, Sub, _3) )
# 793 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                   ( BinOp(_1, Mult, _3) )
# 801 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                    ( BinOp(_1, Div, _3) )
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                 ( BinOp(_1, Mod, _3) )
# 817 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                ( BinOp(_1, Eq, _3) )
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                  ( BinOp(_1, NEq, _3) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                ( BinOp(_1, Less, _3) )
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                 ( BinOp(_1, LEq, _3) )
# 849 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                ( BinOp(_1, Greater, _3) )
# 857 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                 ( BinOp(_1, GEq, _3) )
# 865 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                 ( Assign(_1, BinOp(_1, Add, IntLiteral(1))) )
# 872 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                   ( Assign(_1, BinOp(_1, Sub, IntLiteral(1))) )
# 879 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
              ( TODO() )
# 886 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
             ( TODO() )
# 893 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
              ( TODO() )
# 900 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
              ( TODO() )
# 907 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                             ( _2 )
# 914 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 149 "parser.mly"
                                       ( TODO() )
# 922 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
 ( [] )
# 928 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 153 "parser.mly"
                ( List.rev _1 )
# 935 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
      ( [_1] )
# 942 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
                           ( _3 :: _1 )
# 950 "parser.ml"
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
