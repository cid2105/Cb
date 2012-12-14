open Ast
open Printf

module NameMap = Map.Make(struct
    type t = string
    let compare x y = Pervasives.compare x y
end)

type note = {
    mutable pitch : int;
    mutable octave : int;
    mutable duration : int;
}

type chord = {
    mutable notelist : note list;
    mutable chord_duration : int;
}

type scale = {
    mutable scale_notelist : note list;
}

(*Assumes notes in a stanza are auto-converted to chords*)
type stanza = {
    mutable chordlist : chord list;
}

type score = {
    mutable stanzalist : stanza list;
}

(*
type void = {

}
*)

type cb_type =   Int of int | Bool of bool | Note of note | Chord of chord | Scale of scale | Stanza of stanza | Score of score

let getType v =
    match v with
        Int(v) -> "int"
        | Bool(v) -> "bool"
        | Note(v) -> "note"
        | Chord(v) -> "chord"
        | Scale(v) -> "scale"
        | Stanza(v) -> "part"
        | Score(v) -> "score"

let getInt v =
    match v with
        Int(v) -> v
        | _ -> 0

let getBool v =
    match v with
        Bool(v) -> v
        | _ -> false

let getNote v =
    match v with
        Note(v) -> v
        | _ -> {pitch=128; octave=0; duration=0}

let getChord v =
    match v with
        Chord(v) -> v
        | _ -> {notelist=[]; chord_duration=0}

let getBool v =
    match v with
        Bool(v) -> v
        | _ -> false

let initIdentifier t =
  match t with
    "int" -> Int(0)
    | "bool" -> Bool(false)
    | "note" -> Note({pitch=128; octave=0; duration=0})
    | "chord" -> Chord({notelist=[]; chord_duration=0})
    | "scale" -> Scale({scale_notelist=[]})
    | "stanza" -> Stanza({chordlist=[]})
    | "score" -> Score({stanzalist=[]})
    | _ -> Bool(false)

let setPitch v a = ((getNote v).pitch <- a); v

let noteMap = NameMap.empty

let initNoteMap =
    NameMap.add "C" 0 noteMap;
    NameMap.add "B#" 0 noteMap;
    NameMap.add "C#" 1 noteMap;
    NameMap.add "Db" 1 noteMap;
    NameMap.add "D" 2 noteMap;
    NameMap.add "Eb" 3 noteMap;
    NameMap.add "D#" 3 noteMap;
    NameMap.add "E" 4 noteMap;
    NameMap.add "Fb" 4 noteMap;
    NameMap.add "F" 5 noteMap;
    NameMap.add "E#" 5 noteMap;
    NameMap.add "F#" 6 noteMap;
    NameMap.add "Gb" 6 noteMap;
    NameMap.add "G" 7 noteMap;
    NameMap.add "Ab" 8 noteMap;
    NameMap.add "G#" 8 noteMap;
    NameMap.add "A" 9 noteMap;
    NameMap.add "Bb" 10 noteMap;
    NameMap.add "A#" 10 noteMap;
    NameMap.add "B" 11 noteMap;
    NameMap.add "Cb" 11 noteMap;

exception ReturnException of cb_type * cb_type NameMap.t

(*this will need to be passed around*)
let func_decls = NameMap.empty
let csv = ""

let rec eval env = function
    Id(name) -> print_string ("I am an id with name: " ^ name ^ "\n");
        let locals, globals = env in
            if NameMap.is_empty globals then
                raise (Failure ("Fuck, globals is empty"))
            else if NameMap.mem name locals then
                (NameMap.find name locals), env
            else if NameMap.mem name globals then
                (NameMap.find name globals), env
            else raise (Failure ("Undeclared identifier: " ^ name))
    | MemberAccess(vname, memname) -> print_string ("I am a member access on var: " ^ vname ^ " member: " ^ memname ^ "\n");
        let v, env = eval env (Id vname) in
            let vType = getType v in
            (match vType with
                | "note" ->
                    (match memname with
                        "pitch" -> Int (getNote v).pitch
                        | "octave" -> Int (getNote v).octave
                        | "duration" -> Int (getNote v).duration
                        | _ -> raise (Failure ("invalid property of note: " ^ memname)))
                | "chord" ->
                    (match memname with
                        "duration" -> Int (getChord v).chord_duration
                        | _ -> raise (Failure ("invalid property of chord: " ^ memname)))
                | _ -> raise (Failure ("cannot access " ^ vname ^ "." ^ memname))), env
    | IntLiteral(i) -> print_string ("I am an intliteral: " ^ (string_of_int i) ^ "\n"); (Int i, env);
    | NoteConst(s) -> print_string ("I am a note constant: " ^ s ^ "\n");
        Int (NameMap.find s noteMap), env
    | BoolLiteral(b) -> print_string ("I am a bool literal: " ^ (string_of_bool b) ^ "\n"); (Bool b, env)
    | ChordExpr(el, e) -> print_string ("I am a chord expression: \n"); 
        List.iter (fun a -> 
        (let chord_elem, env = eval env a in
            let vType = getType( chord_elem ) in
                if ( vType = "note") then raise (Failure ("Duration does not evaluate to an integer"))
        )) el; 
        (Chord ({notelist=[]; chord_duration=0}), env)
    | DurConst(s) -> print_string ("I am a duration constant: " ^ s ^ "\n");
        if s = "whole" then Int 64, env
            else if s = "half" then Int 32, env
            else if s = "quarter" then Int 16, env
            else raise (Failure ("Duration constant unknown"))
    | NoteExpr(s,e,e1) -> print_string ("I am a note expression: " ^ s ^ "," ^ "\n");
        let oct, env = eval env e in
            let octType = getType oct in
                if octType = "int" then (let dur, env = eval env e1 in
                                    let durType = getType dur in
                                        if durType = "int" then (Note ({pitch=(NameMap.find s noteMap); octave=(getInt oct); duration=(getInt dur)}), env)
                                        else raise (Failure ("Duration does not evaluate to an integer")))
                else  raise (Failure ("Octave does not evaluate to an integer"))
    | BinOp(e1,o,e2) -> print_string ("I am a binary operation\n");
        let v1, env = eval env e1 in
        let v2, env = eval env e2 in
        let v1Type = getType v1 in
        let v2Type = getType v2 in
        (* Two variables have to be of the same type for binop *)
        if v1Type = v2Type then
            (match o with (* Only accept ints for now *)
                Add ->
                    if v1Type = "int" then
                    Int (getInt v1 + getInt v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " + " ^ v2Type))
                | Sub ->
                    if v1Type = "int" then
                    Int (getInt v1 - getInt v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " - " ^ v2Type))
                | Mult ->
                    if v1Type = "int" then
                    Int (getInt v1 * getInt v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " * " ^ v2Type))
                | Div ->
                    if v1Type = "int" then
                    Int (getInt v1 / getInt v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " / " ^ v2Type))
                | Mod ->
                    if v1Type = "int" then
                    Int (getInt v1 mod getInt v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " % " ^ v2Type))
                | And ->
                    if v1Type = "bool" then
                    Bool (getBool v1 && getBool v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " and " ^ v2Type))
                | Or ->
                    if v1Type = "bool" then
                    Bool (getBool v1 || getBool v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " or " ^ v2Type))
                | Eq ->
                    if v1Type = "int" then
                    Bool (getInt v1 = getInt v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " is " ^ v2Type))
                | NEq ->
                    if v1Type = "int" then
                    Bool (getInt v1 != getInt v2)
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " isnt " ^ v2Type))
                | _ -> raise (Failure ("Unknown binary operation"))
                | Less ->
                    if v1Type = "int" then
                        Bool (getInt v1 < getInt v2)
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " < " ^ v2Type))
                | LEq ->
                    if v1Type = "int" then
                        Bool (getInt v1 <= getInt v2)
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " <= " ^ v2Type))
                | Greater ->
                    if v1Type = "int" then
                        Bool (getInt v1 > getInt v2)
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " > " ^ v2Type))
                | GEq ->
                    if v1Type = "int" then
                        Bool (getInt v1 >= getInt v2)
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " >= " ^ v2Type))
                (* | IDTimes -> ), env *)
            ), env
        else raise (Failure ("type mismatch: " ^ v1Type ^ " and " ^ v2Type))
    | MethodCall("print", [e]) ->
        let v, env = eval env e in
          (if getType v = "int" then
            print_endline (string_of_int (getInt v))
          else if getType v = "bool" then
            print_endline (string_of_bool (getBool v))
          else
            print_endline(getType v));
          (Bool false), env      
    | UnaryOp(uo,e) -> print_string ("I am a unary operation\n");
        let v, env = eval env e in
        let vType = getType v in
        if ( vType = "note" or vType = "chord" ) then
            (match uo with (* Only accept notes for now *)
                Raise ->
                    if vType = "note" then
                        setPitch v ((getNote v).pitch + 1)
                    else
                        raise (Failure ("cannot raise: " ^ vType))
                | Lower ->
                    if vType = "note" then
                        setPitch v ((getNote v).pitch - 1)
                    else
                        raise (Failure ("cannot lower: " ^ vType))
            ), env
        else raise (Failure ("type mismatch: " ^ vType ^ " is not suitable, must be a note or chord"))
    (* | ListExpr([el]) -> print_string ("I am a list epxression\n") *)
    (* | MethodCall(s,el) -> print_string ("I am a method call on: " ^ s ^ "\n") *)
    (* | Assign(toE, fromE) -> print_string ("I am an assignment\n") *)
    | NoExpr -> print_string ("I am nothingness\n"); Bool true, env

(* Main entry point: run a program *)
let rec run prog env =
    let locals, globals = env in
        if NameMap.is_empty globals then print_string ("In run, globals is empty\n") else print_string ("In run, globals in non-empty\n");
        match prog with
            [] -> print_string "Fuck it I'm done\n"
            | head::tail ->
                match head with
                    VDecl(head) -> print_string ("Variable Declaration: " ^ head.varname ^ "\n");
                        run tail (locals, (NameMap.add head.varname (initIdentifier "note") globals));
                    | FullDecl(head) -> print_string ("Full Declaration: " ^ head.fvname ^ "\n"); run tail (locals, globals)
                    | MDecl(head) -> print_string ("Method Declaration: " ^ head.fname ^ "\n"); (NameMap.add head.fname head func_decls); run tail (locals, globals)
                    | Stmt(head) -> match head with
                                    Expr(e) -> (eval (NameMap.empty, globals) e);
                                                run tail (locals, globals)
                                    | Return(e) -> print_string ("I am an a return statement" ^ "\n"); run tail (locals, globals)
                                    | Block(sl) -> print_string ("I am a block statement" ^ "\n"); run tail (locals, globals)
                                    | If(e, sl, s1, s2) -> print_string ("I am a if statement" ^ "\n"); run tail (locals, globals)
                                    | ElseIf(e, sl) -> print_string ("I am a elseif statement" ^ "\n"); run tail (locals, globals)
                                    | Foreach(p, a, sl) -> print_string ("I am a foreach statement" ^ "\n"); run tail (locals, globals)
                                    | While(e, sl) -> print_string ("I am a while statement" ^ "\n"); run tail (locals, globals)
                                    | _ -> raise (Failure ("Unable to match the statment "))

let helper prog = run prog (NameMap.empty, NameMap.empty)
