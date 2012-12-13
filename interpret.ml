open Ast
open Printf

module NameMap = Map.Make(struct
    type t = string
    let compare x y = Pervasives.compare x y
end)

(*)
type note = {
    mutable pitch : int;
    mutable octave : int;
    mutable duration : int;
}

type chord = {
    mutable notelist : note list;
    mutable duration : int;
}

type scale = {
    mutable notelist : note list;
}

(*Assumes notes in a stanza are auto-converted to chords*)
type stanza = {
    mutable chordlist : chord list;
}

type score = {
    mutable stanzalist : stanza list;
}
*)
type cbtype =   Int of int

(*)
                | Bool of bool
                | Note of note
                | Chord of chord
                | Scale of scale
                | Stanza of stanza
                |Score of score
*)



let getType v =
    match v with
        Int(v) -> "int"
(*      | Bool(v) -> "bool"
        | Note(v) -> "note"
        | Chord(v) -> "chord"
        | Scale(v) -> "scale"
        | Stanza(v) -> "stanza"
        | Score(v) -> "score" *)

let getInt v =
    match v with
        Int(v) -> v
        | _ -> 0

let initIdentifier t =
  match t with
    "int" -> Int(0) (*)
    | "bool" -> Bool(false)
    | "note" -> Note({pitch=128; intensity=0; duration=0.0})
    | "chord" -> Chord({notelist=[]})
    | "staff" -> Staff({instrument=0; chordlist=[]})
    | "part" -> Part({bpm=90; beatsig=0.25; stafflist=[]})
    | _ -> Bool(false)
*)

exception ReturnException of cbtype * cbtype NameMap.t

let func_decls = NameMap.empty
let globals = NameMap.empty
let csv = ""

(* Main entry point: run a program *)
let rec run prog =
    [] -> Printf.printf "Fuck it I'm done"
    | head::tail ->
        match head with
        VDecl -> (NameMap.add head.varname (initIdentifier head.vartype) globals); run tail
        | MDecl -> (NameMap.add head.fname head func_decls); run tail
        | Stmt -> let fuck = (* Call exec on head, then recursively run tail *)
    | _ -> raise (Failure ("You broke the run function"))

    (* Invoke a function and return an updated global symbol table *)
    let rec call methdecl actuals globals =

        (* Evaluate an expression and return (value, updated environment) *)
        let rec eval env = function
            IntLiteral(i) -> Int i, env
            | Id(var) ->
                let locals, globals = env in
                (* If the Id is in a local scope *)
                if NameMap.mem var locals then
                    (NameMap.find var locals), env
                (* If the Id is in a global scope *)
                else if NameMap.mem var globals then
                    (NameMap.find var globals), env
                else raise (Failure ("undeclared identifier " ^ var))
            | Assign(var, e) ->
                (* lhs_expr: is the left hand side of the assignment operation
                          after being evaluated
                    rhs_expr: is the right hand isde of the assignment operation
                          after being
                *)
                (* Calling eval on the environment and the left hand sign of assignment*)
                let lhs_expr, env = eval env var in
                (* Calling eval on the environment and the right hand sign of assignment*)
                let rhs_expr, (locals, globals) = eval env e in
                (* match the var with an id or member access *)
                let lhs_Info =
                    match var with
                        Id (i) -> ("id", (i, ""))
                        | MemberAccess(i, j) -> ("member", (i, j))
                     | _ -> raise (Failure ("left side of assignment must be an identifier or member access")) in
                (* The first tuple representing the type, id or member*)
                let lhs_Id_type = fst lhs_Info in
                (* The second tuple representing the name (i, "") or (i, j)*)
                let lhs_name = snd lhs_Info in
                (* No clue what the fuck this is doing *)
                let lhs_type = (* ("note", "locals") *)
                    (if NameMap.mem (fst lhs_name) locals then
                        (getType (NameMap.find (fst lhs_name) locals), "locals")
                    else if NameMap.mem (fst lhs_name) globals then
                        (getType (NameMap.find (fst lhs_name) globals), "globals")
                    else raise (Failure ("undeclared identifier: " ^ fst lhs_name)))
                in
                (* get the type of what you are assigning (left hand side) *)
                let lhs_return_type = getType lhs_expr in
                (* get the type of what you are assigning to (right hand side) *)
                let rhs_return_type = getType rhs_expr in
                    (* If the types of the left and right hand sides match continue *)
                    if lhs_return_type = rhs_return_type then
                        match lhs_return_type with
                        "int" ->
                            if lhs_Id_type = "id" then
                                (if snd lhs_type = "locals" then
                                    rhs_expr, (NameMap.add (fst lhs_name) rhs_expr locals, globals)
                                else if snd lhs_type = "globals" then
                                    rhs_expr, (locals, NameMap.add (fst lhs_name) rhs_expr globals)
                                else raise (Failure ("fatal error")))
                            (* PUT IN ELSE IF FOR TYPE BEING MEMBER, PlaceHOLDER *)
                            else if lhs_Id_type = "member" then
                                raise (Failure ("You suck big time bro"))
                            else raise (Failure ("Unable to match left hand side of assignment"))
                    else if lhs_Id_type = "id" then
                        raise (Failure ("cannot assign: " ^ fst lhs_type ^ " = " ^ rhs_return_type))
                    else if lhs_Id_type = "member" then
                        raise (Failure ("cannot assign: " ^ lhs_return_type ^ " = " ^ rhs_return_type))
                    else raise (Failure ("fatal error"))
        in

        (* Placeholder code, need to change later *)
        let rec exec env = function
            Expr(e) -> let _, env = eval env e in env
        in


        (* Enter the function: bind actual values to formal arguments *)
        let locals =
            try List.fold_left2
                (fun locals formal actual -> NameMap.add formal.paramname actual locals)
                NameMap.empty methdecl.formals actuals
            with Invalid_argument(_) ->
             raise (Failure ("wrong number of arguments passed to " ^ methdecl.fname))
        in

        (* Execute each statement in sequence, return updated global symbol table *)
        snd (List.fold_left exec (locals, globals) methdecl.body)

        (* Run a program: initialize global variables to 0, find and run "main" *)
    in
    let globals = NameMap.empty
    in snd (List.fold_left exec (locals, globals) program)







