type op =
    Add | Sub | Mult | Div | Mod
    | And | Or | Eq | NEq | Less | LEq | Greater | GEq | IDTimes

type uop =
    Sharp | Flat | Raise | Lower

type cb_type =
    Void | Int | Note | Bool | Chord | Scale | Stanza | Score

type expr = (* Expressions *)
    Id of string (* foo *)
    (*| Cbtype of string mn stand for Datatype*)
    | MemberAccess of string * string (* foo.intensity *)
    | IntLiteral of int (* 42 *)
    | DurInt of int (*mn always > 0 *)
    | NoteConst of string (*mn A, B#, C, ...*)
    | BoolLiteral of bool (* true *)
    | DurConst of string (*mn whole, half, ... *)
    (*| ElemOp of string * expr (*a[5]*)*)
    | Assign of expr * expr (* x = y *)
    | TypeAssign of string * expr  (* Note a = .... *)
    | NoteExpr of string * int * expr (*mn x = (A#, 5>octave>-5, 4 + 1 ) *)
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

type stmt = (* Statements *)
    Expr of expr (* foo = bar + 3; *)
    | Return of expr (* return 42; *)
    | Block of stmt list (* ) ... end *)
    | If of expr * stmt list * stmt * stmt (*mn if (foo isnt 42) ... elseif(foo > 42 ) ... else ... end *)
    | ElseIf of expr * stmt list (*mn elseif(x is 5) ..... end *)
    | Foreach of par_decl * string * stmt list (*mn foreach (x in nots) ... end *)
    | While of expr * stmt list(*mn while (i<10) ... end *)

type var_decl = {
    varname : string; (* Name of the variable *)
    vartype : cb_type; (* Name of variable type *)
}

type meth_decl = {
    fname : string; (* Name of the function *)
    rettype : cb_type; (* Name of return type *)
    formals : par_decl list; (* Formal argument names *)
    locals : var_decl list; (* Locally defined variables *)
    body : stmt list;
}

type generic = {
    meth_decl
    | stmt
    | var_decl
}

type program = generic list