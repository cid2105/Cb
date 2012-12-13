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
        | Assign(e, e) ->
        
        | TypeAssign(var, e) ->
            let v, (locals, globals) = eval env e in
            if NameMap.mem var locals then
                v, (NameMap.add var v locals, globals)
            else if NameMap.mem var globals then
                v, (locals, NameMap.add var v globals)
            else raise (Failure ("undeclared identifier " ^ var))








