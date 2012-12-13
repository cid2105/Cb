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

(*)

let getType v =
    match v with
        Int(v) -> "int"
        | Bool(v) -> "bool"
        | Note(v) -> "note"
        | Chord(v) -> "chord"
        | Scale(v) -> "scale"
        | Stanza(v) -> "stanza"
        | Score(v) -> "score"

let getInt v =
    match v with
        Int(v) -> v
        | _ -> 0

let initIdentifier t =
  match t with
    "int" -> Int(0)

(* Main entry point: run a program *)
let run (vars, funcs) = 

    (* Put function declarations in a symbol table *)
    let func_decls = List.fold_left
        (fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
        NameMap.empty funcs
    in
    
    (* Invoke a function and return an updated global symbol table *)
    let rec call fdecl actuals globals =
    
    (* Evaluate an expression and return (value, updated environment) *)
    let rec eval env = function
        IntLiteral(i) -> Int i, env
        | Id(var) ->
            let locals, globals = env in
            if NameMap.mem var locals then  (* If the Id is a member of the locals (local scope) *)
                (NameMap.find var locals), env
            else if NameMap.mem var globals then (* If the Id is a member of the globals (global scope) *)
                (NameMap.find var globals), env
            else raise (Failure ("undeclared identifier: " ^ var)) (* If the Id is nonexistent error! *)
        | TypeAssign() ->
        | Assign(var, h) ->
*)

exception ReturnException of cbtype * cbtype NameMap.t

(* Main entry point: run a program *)
let run (var, funcs) =

    (* Put function declarations in a symbol table *)
    let func_decls = List.fold_left
        (fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
        NameMap.empty funcs
    in

    (* Invoke a function and return an updated global symbol table *)
    let rec call fdecl actuals globals =

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
            (* Calling eval on the environment and the left hand sign of assignment*)
            let var1, env = eval env var in
            (* Calling eval on the environment and the right hand sign of assignment*)
            let e1, (locals, globals) = eval env e in
            (* match the var with an id or member access *)
            let v1Info =   
                match var with
                    Id (i) -> ("id", (i, ""))
                    | MemberAccess(i, j) -> ("member", (i, j))
                 | _ -> raise (Failure ("left side of assignment must be an identifier or member access")) in         
            (* The first tuple representing the type, id or member*)
            let v1IdType = fst v1Info in
            (* The second tuple representing the name (i, "") or (i, j)*)
            let v1Name = snd v1Info in
            (* No clue what the fuck this is doing *)
            let v1Type = (* ("note", "locals") *)
                (if NameMap.mem (fst v1Name) locals then
                    (getType (NameMap.find (fst v1Name) locals), "locals")
                else if NameMap.mem (fst v1Name) globals then
                    (getType (NameMap.find (fst v1Name) globals), "globals")
                else raise (Failure ("undeclared identifier: " ^ fst v1Name)))                   
            in


            if NameMap.mem var locals then
                v, (NameMap.add var v locals, globals)
            else if NameMap.mem var globals then
                v, (locals, NameMap.add var v globals)
            else raise (Failure ("undeclared identifier " ^ var))
    in

    (* Placeholder code, need to change later *)
    let rec exec env = function
        Block(stmts) -> List.fold_left exec env stmts 
    in


    (* Enter the function: bind actual values to formal arguments *)
    let locals =
        try List.fold_left2
            (fun locals formal actual -> NameMap.add formal actual locals)
            NameMap.empty fdecl.formals actuals
        with Invalid_argument(_) ->
            raise (Failure ("wrong number of arguments passed to " ^ fdecl.fname))
    in

    (* Initialize local variables to 0 *)
    let locals = List.fold_left
        (fun locals local -> NameMap.add local 0 locals) locals fdecl.locals
    in
    (* Execute each statement in sequence, return updated global symbol table *)
    snd (List.fold_left exec (locals, globals) fdecl.body)

    (* Run a program: initialize global variables to 0, find and run "main" *)
    in let globals = List.fold_left
        (fun globals vdecl -> NameMap.add vdecl 0 globals) NameMap.empty vars
    in try
        call (NameMap.find "main" func_decls) [] globals

    with Not_found -> raise (Failure ("did not find the main() function"))


















