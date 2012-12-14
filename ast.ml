type op =
    Add | Sub | Mult | Div | Mod
    | And | Or | Eq | NEq | Less | LEq | Greater | GEq | IDTimes

type uop =
    Raise | Lower

type cb_type =
    Void | Int | Note | Bool | Chord | Scale | Stanza | Score

type expr = (* Expressions *)
    Id of string (* foo *)
    (*| Cbtype of string mn stand for Datatype*)
    | MemberAccess of string * string (* foo.intensity *)
    | IntLiteral of int (* 42 *)
    | NoteConst of string (*mn A, B#, C, ...*)
    | BoolLiteral of bool (* true *)
    | DurConst of string (*mn whole, half, ... *)
    (*| ElemOp of string * expr (*a[5]*)*)
    | Assign of expr * expr (* x = y *)
    | NoteExpr of string * expr * expr (*mn x = (A#, 5>octave>-5, 4 + 1 ) *)
    | ChordExpr of expr list * expr (* chord =  *)
    | ListExpr of expr list (*mn x = [a, b*6, c] ???*)
    | BinOp of expr * op * expr (* x + y *)
    | UnaryOp of uop * expr
    | MethodCall of string * expr list (*mn foo(x, y) *)
    | NoExpr (* for (;;) *)


type par_decl = {
    paramname : string; (* Name of the variable *)
    paramtype : cb_type; (* Name of variable type *)
}

type var_decl = {
    varname : string; (* Name of the variable *)
    vartype : cb_type; (* Name of variable type *)
}

type fullvdecl = {
    fvtype : cb_type;
    fvname : string;
    fvexpr : expr;
}

type meth_decl = {
    fname : string; (* Name of the function *)
    rettype : cb_type; (* Name of return type *)
    formals : par_decl list; (* Formal argument names *)
    body : innerblock list; }
and
stmt = (* Statements *)
    Expr of expr (* foo = bar + 3; *)
    | Return of expr (* return 42; *)
    | Block of innerblock list (* ) ... end *)
    | If of expr * innerblock list * stmt * stmt (*mn if (foo isnt 42) ... elseif(foo > 42 ) ... else ... end *)
    | ElseIf of expr * innerblock list (*mn elseif(x is 5) ..... end *)
    | Foreach of par_decl * string * innerblock list (*mn foreach (x in nots) ... end *)
    | While of expr * innerblock list(*mn while (i<10) ... end *)
and
innerblock =
    Stmt2 of stmt
  | FullDecl2 of fullvdecl
  | VDecl2 of var_decl
and
generic =
    Stmt of stmt
  | FullDecl of fullvdecl
  | VDecl of var_decl
  | MDecl of meth_decl

type program =  generic list


let string_of_cbtype cbt = 
    match cbt with
        Note -> "note"
        | Int -> "int"
        | Void -> "void"
        | Chord -> "chord"
        | Bool -> "bool" | Scale -> "scale" | Stanza -> "stanza" | Score -> "score";
        | _ -> "";;

let string_of_uop uop = 
    match uop with
    Raise -> "raise" | Lower -> "lower"
    
let string_of_pdecl var = 
    string_of_cbtype var.paramtype ^ " " ^ var.paramname

let string_of_vdecl var = 
    string_of_cbtype var.vartype ^ " " ^ var.varname ^ ";\n"


let rec string_of_expr = function
    Id(s) -> s
    | MemberAccess(id, mt) -> id ^ "." ^  mt
    | IntLiteral(l) -> string_of_int l
    (*| DurInt(c) -> string_of_int c*)
    | NoteConst(c) -> c
    | BoolLiteral(b) -> string_of_bool b;
    | DurConst(c) -> c
    | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e (* expr = expr ?????? *)
    | NoteExpr(id, e1, e2) -> " ( " ^ id ^ " , " ^ string_of_expr e1 ^ " , " ^ string_of_expr e2 ^ " ) "
    | ChordExpr(l, e) -> "( [ " ^ String.concat " , " (List.map string_of_expr l) ^ "] , " ^ string_of_expr e ^ " ) ";
    (*| TypeAssign(v, e) -> v ^ " = " ^ string_of_expr e (* Chord a = ... *) *)
  (*| ElementOp(s, e1) -> s ^ "[" ^ string_of_expr e1 ^ "]";*)
    | ListExpr(e) -> " [ " ^ String.concat " , " (List.map string_of_expr e) ^ " ] "

(*type op =
    Add | Sub | Mult | Div | Mod
    | And | Or | Eq | NEq | Less | LEq | Greater | GEq | IDTimes*)

    | BinOp(e1, o, e2) -> begin
        string_of_expr e1 ^ " " ^
        (match o with
        Add -> "+" | Sub -> "-"
            (*| DotAdd -> ".+" | DotSub -> ".-" *)
            | Eq -> "==" | NEq -> "!="
            | Less -> "<" | LEq -> "<=" | Greater -> ">" | GEq -> ">="
            | And -> "&&" | Or -> "||" | Mod -> "%" | Mult -> "*" | Div -> "/" | IDTimes -> ".*") ^ 
            " " ^ string_of_expr e2
        end

    | UnaryOp(up, e) -> string_of_uop up ^ " " ^ string_of_expr e
    | MethodCall(f, el) ->
      f ^ "(" ^ String.concat " , " (List.map string_of_expr el) ^ " ) " (*?????*)
    | NoExpr -> "";;

let string_of_fvdl fvdel = 
    string_of_cbtype fvdel.fvtype ^ " " ^ fvdel.fvname ^ " = " ^ string_of_expr fvdel.fvexpr ^ " ;\n"

let rec string_of_stmt = function
    Block(stmts) -> begin
        let rec string_of_abst = function (* inner block list *)
            [] -> ""
            | innerb::abstr ->
                match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n" ^ string_of_abst abstr;
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ^ string_of_abst abstr;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ^ string_of_abst abstr;
        in 
        "\n" ^  string_of_abst stmts ^ "\nend\n" (* ) Block end *)
        end
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> begin 
        match expr with
            NoExpr -> "\n" (* print nothing user has to input Return ; though *)
            | _ -> "return " ^ string_of_expr expr ^ ";\n"; end
    (*| Break -> "break;\n";
        | Continue -> "continue;\n";*)
    | If(e, s, els, Block([])) -> begin
        match els with
            Block([]) -> "if (" ^ string_of_expr e ^ ")\n" ^ String.concat " " (List.map 
                (fun x -> 
                    match x with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )
             s);(* no elsif part given *)
            | _ -> "if (" ^ string_of_expr e ^ ")\n" ^ String.concat "\n" (List.map 

                (fun innerb -> 
                    match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )

            s) ^ "\n" ^ string_of_stmt els; 
        end
    | If(e, s1, els, s2) -> begin 
        match els with
            Block([]) ->    "if (" ^ string_of_expr e ^ ")\n" ^ String.concat " " (List.map 

                 (fun innerb -> 
                    match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )


             s1) ^ 
                            "else\n" ^ string_of_stmt s2;
            | _ ->  "if (" ^ string_of_expr e ^ ")\n" ^ String.concat "\n" (List.map 

                 (fun innerb -> 
                    match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )


             s1) ^ "\n" ^ string_of_stmt els ^ 
                    "else\n" ^ string_of_stmt s2; end
    | ElseIf(e, els) ->  "elsif (" ^ string_of_expr e ^ ")\n" ^ String.concat "\n" (List.map

         (fun innerb -> 
                    match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )


     els)
    | Foreach(p, id, s) ->
        "foreach (" ^ string_of_pdecl p  ^ " in " ^ id ^ " )\n" ^ String.concat "\n" (List.map 

             (fun innerb -> 
                    match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )


         s);

    | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ String.concat "\n" (List.map 

         (fun innerb -> 
                    match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )


     s);;




let string_of_mdecl mdecl =
    mdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_pdecl mdecl.formals) ^ ")\n" ^
   (* String.concat "" (List.map string_of_vdecl mdecl.locals) ^*)
    String.concat "" (List.map 

         (fun innerb -> 
                    match innerb with
                    Stmt2(st) -> string_of_stmt st ^ "\n";
                    | VDecl2(v) -> string_of_vdecl v ^ "\n" ;
                    | FullDecl2(fv) -> string_of_fvdl fv ^ "\n" ;

                )


     mdecl.body) ^ "\nend\n"

let rec string_of_program = function
   [] -> "";
    | generic::gen_lst ->
        match generic with
            VDecl(arg_vd) -> string_of_vdecl arg_vd ^ "" ^ string_of_program gen_lst ;
            | MDecl(arg_mt) -> string_of_mdecl arg_mt ^ "" ^ string_of_program gen_lst ;
            | FullDecl(arg_vd) -> string_of_fvdl arg_vd ^ "" ^ string_of_program gen_lst ;
            | Stmt(arg_st) -> string_of_stmt arg_st ^ "" ^string_of_program gen_lst ;;
