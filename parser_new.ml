type token =
  | INTLITERAL of (int)
  | STRING of (string)
  | DATATYPE of (string)
  | ID of (string)
  | ASSIGN
  | SEMICOLON

open Parsing;;
let _ = parse_error;;
# 1 "parser_new.mly"
 open Ast_tmp 
# 14 "parser_new.ml"
let yytransl_const = [|
  261 (* ASSIGN *);
  262 (* SEMICOLON *);
    0|]

let yytransl_block = [|
  257 (* INTLITERAL *);
  258 (* STRING *);
  259 (* DATATYPE *);
  260 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\000\000"

let yylen = "\002\000\
\000\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\002\000\000\000\003\000"

let yydgoto = "\002\000\
\003\000\005\000"

let yysindex = "\255\255\
\000\000\000\000\254\254\255\254\000\000\252\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000"

let yytablesize = 4
let yytable = "\001\000\
\004\000\007\000\006\000\004\000"

let yycheck = "\001\000\
\003\001\006\001\004\001\000\000"

let yynames_const = "\
  ASSIGN\000\
  SEMICOLON\000\
  "

let yynames_block = "\
  INTLITERAL\000\
  STRING\000\
  DATATYPE\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 16 "parser_new.mly"
( [], [] )
# 73 "parser_new.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast_tmp.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 17 "parser_new.mly"
                ( (_2 :: fst _1), snd _1 )
# 81 "parser_new.ml"
               : Ast_tmp.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 19 "parser_new.mly"
                             ({ vartype = _1; varname = _2})
# 89 "parser_new.ml"
               : 'vdecl))
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
