

type op =
    Add | Sub | Mult | Div | Mod 
    | Is | Isnt | Less | LEq | Greater | GEq 
   
type expr = (* Expressions *)
    Id of string (* foo *)
    | MemberAccess of string * string (* foo.intensity *)   
    | Literal of int (* 42 *)
    | Assign of expr * expr (* x = y *)
    | BinOp of expr * op * expr (* x + y *)
    | Not of expr (* !x *)
    | Call of string * expr list (* foo(x, y) *)
    | NoExpr (* for (;;) *)
    
type stmt = (* Statements *)
    Expr of expr (* foo = bar + 3; *)
    | Return of expr (* return 42; *)
    | Block of stmt list (* { ... } *)
    | If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
    | For of expr * expr * expr * stmt (* for (i=0;i<10;i=i+1) { ... } *)
    | While of expr * stmt (* while (i<10) { i = i + 1 } *)

type par_decl = {
    paramname : string; (* Name of the variable *)
    paramtype : string; (* Name of variable type *)
}

type var_decl = {
    varname : string; (* Name of the variable *)
    vartype : string; (* Name of variable type *)
}

type func_decl = {
    fname : string; (* Name of the function *)
    rettype : string; (* Name of return type *)
    formals : par_decl list; (* Formal argument names *)
    locals : var_decl list; (* Locally defined variables *)
    body : stmt list;
}

type program = var_decl list * func_decl list (* global vars, funcs *)
