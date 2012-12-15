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

type stanza = {
    mutable chordlist : chord list;
}

type score = {
    mutable stanzalist : stanza list;
}

type cb_type =   Int of int | Bool of bool | Note of note | Chord of chord | Scale of scale | Stanza of stanza | Score of score

exception ReturnException of cb_type * cb_type NameMap.t

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

let getScale v =
    match v with
        Scale(v) -> v
        | _ -> {scale_notelist=[]}

let getStanza v =
    match v with
        Stanza(v) -> v
        | _ -> {chordlist=[]}

let getScore v =
    match v with
        Score(v) -> v
        | _ -> {stanzalist=[]}

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
let setDuration v a = ((getNote v).duration <- a); v


(* let noteMap = NameMap.empty in  *)

(* type 'a ref = { mutable content : 'a }
let ref x = { content = x }
let deref r = r.content
let assign r x = r.content <- x; x *)


 let noteMap =
     NameMap.add "C" 0 NameMap.empty
    let noteMap = NameMap.add "B#" 0 noteMap
    let noteMap = NameMap.add "C#" 1 noteMap
    let noteMap = NameMap.add "Db" 1 noteMap
    let noteMap = NameMap.add "D" 2 noteMap
    let noteMap = NameMap.add "Eb" 3 noteMap
    let noteMap = NameMap.add "D#" 3 noteMap
    let noteMap = NameMap.add "E" 4 noteMap
    let noteMap = NameMap.add "Fb" 4 noteMap
    let noteMap = NameMap.add "F" 5 noteMap
    let noteMap = NameMap.add "E#" 5 noteMap
    let noteMap = NameMap.add "F#" 6 noteMap
    let noteMap = NameMap.add "Gb" 6 noteMap
    let noteMap = NameMap.add "G" 7 noteMap
    let noteMap = NameMap.add "Ab" 8 noteMap
    let noteMap = NameMap.add "G#" 8 noteMap
    let noteMap = NameMap.add "A" 9 noteMap
    let noteMap = NameMap.add "Bb" 10 noteMap
    let noteMap = NameMap.add "A#" 10 noteMap
    let noteMap = NameMap.add "B" 11 noteMap
    let noteMap = NameMap.add "Cb" 11 noteMap

(*this will need to be passed around*)
let csv = ""
let csv_head = ""

(* A ref is the simplest mutable data structure. *)
let tick : int ref = ref 0

(* let getNoteList cbtypelist = List.map ( fun a -> Note( getNote a ) ) cbtypelist

let getChordList cbtypelist = List.map ( fun a -> Chord( getChord a ) ) cbtypelist

let getStanzaList cbtypelist = List.map ( fun a -> Stanza( getStanza a ) ) cbtypelist
 *)

let rec eval env = function
    Id(name) -> print_string ("I am an id with name: " ^ name ^ "\n");
        let locals, globals, fdecls = env in
            if NameMap.is_empty globals then
                raise (Failure ("Fuck, globals is empty"))
            else if NameMap.mem name locals then
                (NameMap.find name locals), env
            else if NameMap.mem name globals then
                (NameMap.find name globals), env
            else raise (Failure ("undeclared identifier: " ^ name))
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
                  | _ -> raise (Failure ("invalid property of staff: " ^ memname)))
              | _ -> raise (Failure ("cannot access " ^ vname ^ "." ^ memname))), env
    | IntLiteral(i) -> print_string ("I am an intliteral: " ^ (string_of_int i) ^ "\n");
        (Int i, env);
    | NoteConst(s) -> print_string ("I am a note constant: " ^ s ^ "\n");
        Int (NameMap.find s noteMap), env
    | BoolLiteral(b) -> print_string ("I am a bool literal: " ^ (string_of_bool b) ^ "\n");
        (Bool b, env)
    | ChordExpr(el, e) -> print_string ("I am a chord expression: \n");
        let note_list = List.map (fun (note_elem) ->
            (let chord_elem, env = eval env note_elem in
                let vType = (getType chord_elem) in
                    if ( vType = "note") then (getNote (chord_elem))
                    else raise (Failure ("Chord must be composed of notes "))
            )) el in
                let dur, env = eval env e in
                    let durType = getType dur in
                        if durType = "int" then (Chord ({notelist=note_list; chord_duration=(getInt dur)}), env)
                        else raise (Failure ("Duration does not evaluate to an integer"))
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
                                        if durType = "int" then
                                        begin
                                            print_string ("this ans: " ^ (string_of_int (getInt oct)) ^ "\n");
                                            print_string ("this ans: " ^ (string_of_bool (NameMap.is_empty noteMap)) ^ "---" ^(string_of_int (getInt dur))^"\n");
                                            (Note ({pitch=(NameMap.find s noteMap); octave=(getInt oct); duration=(getInt dur)}), env);
                                        end
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
        let arg, env = eval env e in
            (if getType arg = "int" then
                print_endline (string_of_int (getInt arg))
            else if getType arg = "bool" then
                print_endline (string_of_bool (getBool arg))
            else
                print_endline(getType arg));
            (Bool false), env
    | MethodCall("randint", [e]) ->
            let v, env = eval env e in
                if getType v = "int" then
                    Int(Random.int (getInt v)), env
                else raise (Failure ("argument of randint must be an integer"))
    (* assume you get notes only/ no error checking yet ex: [3, 4, 5] is not checked *)
    | MethodCall("compose", [e]) -> (* Writes the specified part to a java file to be written into midi *)
            ignore (match e with
                        Id(i) -> i
                        | _ ->  raise (Failure ("compose takes an identifier as input")));
            let ee1, env = eval env e in
                (if getType ee1 = "score" then
                    let pp = getScore(ee1) in
                    (let headers = csv_head in
                        let csvf = open_out "musicfi.csv" in
                            (fprintf csvf "%s" headers;
                            (* note a = (C, 1, half) csv format => placement(0,4,8...), duration(half), pitch(C) *)
                            let print_note nt =
                                fprintf csvf "%s\n" ( (string_of_int !tick) ^ "," ^
                                                    (string_of_int nt.duration) ^ "," ^
                                                    (string_of_int nt.pitch));
                                (tick := !tick + nt.duration )
                            in
                            let print_chord cd =
                                List.iter (fun nt ->
                                            fprintf csvf "%s\n" ( (string_of_int !tick) ^ "," ^
                                                            (string_of_int cd.chord_duration) ^ "," ^
                                                            (string_of_int nt.pitch));
                                            );
                                (tick := !tick + cd.chord_duration)
                                       (*  let a = (List.map (fun nt -> (nt.duration <- cd.chord_duration) ) cd.notelist) in
                                        begin   print_note ((List.hd  a));
                                    end *)
                            in
                            let print_stanza stan =
                                    if (List.length stan.chordlist = 1) then
                                        begin
                                            let nt = (List.hd stan.chordlist);
                                            in print_note (List.hd nt.notelist );
                                        end
                                    else
                                        List.iter  print_chord (List.rev stan.chordlist);
                            in
                        List.iter print_stanza (List.rev pp.stanzalist)
                    );
                    close_out csvf);
                    ee1
                    , env
                else raise (Failure ("compose takes a score only")));
    | MethodCall(name, el) -> print_string ("Calling Method: " ^ name ^ "\n");
        let locals, globals, fdecls = env in
            let fdecl = try (NameMap.find name fdecls)
                        with Not_found -> raise (Failure ("Undefined function: " ^ name))
            in
                let actuals, env = List.fold_left
                    (fun (al, env) actual ->
                        let v, env = ((eval env) actual) in (v :: al), env
                    ) ([], env) el
                in
                    let locals =
                        try List.fold_left2 (fun locals formal actual ->
                                                if (getType actual) = (string_of_cbtype formal.paramtype) then
                                                    (NameMap.add formal.paramname actual locals)
                                                else
                                                    raise (Failure ("Wrong parameter type in method call to " ^ fdecl.fname))
                                            ) NameMap.empty fdecl.formals actuals
                        with Invalid_argument(_) -> raise (Failure ("wrong number of arguments to: " ^ fdecl.fname))
                    in
                    begin
                        try
                            let globals =
                                (call fdecl.body locals globals fdecls name) in
                                    Bool false, (locals, globals, fdecls) (* This gets hit if you never see a return statement *)
                       with ReturnException(v, g) -> v, (locals, g, fdecls) (* This gets hit if you hit a return statement *)
                   end
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
    | ListExpr(el) -> print_string ("I am a list epxression\n"); (*  el is elment list *)
        let master, _ = (eval env (List.hd el)) in (* pull of the first element in el and evalute *)
            let master_type = (getType master) in (* the type of the first element, everything gets compared to this *)
            begin
                match master_type with (* what is the master type? *)
                    "note" -> (* if it is a note create a scale *)
                        let note_list = List.map (fun (list_elem) ->  (* apply eval func to each elemnt of el *)
                            (let evaled, env = eval env list_elem in
                                let vType = (getType evaled) in
                                    if (vType = "note") then
                                        getNote evaled (* return a note *)
                                    else raise (Failure ("List expressions must contain elements of only 1 type")) (* not a note break *)
                            )) el in
                                (Scale ({scale_notelist = note_list}), env);
                    | "chord" -> (* if it is a chord create a stanza *)
                        let chord_list = List.map (fun (list_elem) ->
                            (let evaled, env = eval env list_elem in
                                let vType = (getType evaled) in
                                    if (vType = "chord") then
                                        (* (Chord (getChord(evaled))) *)
                                        getChord evaled
                                    else raise (Failure ("List expressions must contain elements of only 1 type"))
                            )) el in
                                (Stanza ({chordlist = chord_list}), env);
                    | "stanza" -> (* if it is a stanza create a score *)
                        let stanza_list = List.map (fun (list_elem) ->
                            (let evaled, env = eval env list_elem in
                                let vType = (getType evaled) in
                                    if (vType = "stanza") then
                                        getStanza evaled
                                    else raise (Failure ("List expressions must contain elements of only 1 type"))
                            )) el in
                                (Score ({stanzalist = stanza_list}), env);
                    | _ -> raise (Failure ("List expression must only contain notes or chords or stanzas"))
            end
    | Assign(toE, fromE) -> print_string ("I am an assignment\n");
        let lft_expr, env = eval env toE in
            let rht_expr, (locals, globals, fdecls) = eval env fromE in
                let lftInfo =
                    match toE with
                        Id(i) -> ("id", (i, ""))
                        | MemberAccess(i, j) -> ("member", (i, j))
                        | _ -> raise (Failure ("left side of assignment must be an identifier or member access")) in
                let lftIdType = fst lftInfo in
                    let lftName = snd lftInfo in
                        let lftType = (* ("note", "locals") *)
                            (if NameMap.mem (fst lftName) locals then
                                (getType (NameMap.find (fst lftName) locals), "locals")
                            else if NameMap.mem (fst lftName) globals then
                                (getType (NameMap.find (fst lftName) globals), "globals")
                            else raise (Failure ("undeclared identifier: " ^ fst lftName))) in
                        let lftRetType = getType lft_expr in
                            let rhtType = getType rht_expr in
                            if lftRetType = rhtType then
                                match lftRetType with
                                    "int" ->
                                        if lftIdType = "id" then
                                            (if snd lftType = "locals" then
                                                rht_expr, (NameMap.add (fst lftName) rht_expr locals, globals, fdecls)
                                            else if snd lftType = "globals" then
                                                rht_expr, (locals, NameMap.add (fst lftName) rht_expr globals, fdecls)
                                            else raise (Failure ("fatal error")))
                                        else if lftIdType = "member" then
                                            if fst lftType = "note" then
                                                if snd lftName = "pitch" then
                                                    if getInt rht_expr >= 0 && getInt rht_expr <= 127 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).pitch <- getInt rht_expr); (locals, globals, fdecls))
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).pitch <- getInt rht_expr); (locals, globals, fdecls))         
                                                        else raise (Failure ("fatal error"))
                                                    else raise (Failure ("invalid note pitch: " ^ string_of_int (getInt rht_expr) ^ ". pitch must be between 0-127."))
                                                else if snd lftName = "duration" then (* min max checking *)
                                                    if getInt rht_expr >= 0 && getInt rht_expr <= 127 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).duration <- getInt rht_expr); (locals, globals, fdecls))
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).duration <- getInt rht_expr); (locals, globals, fdecls))         
                                                        else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                    else raise (Failure ("invalid note duration: " ^ string_of_int (getInt rht_expr) ^ ". duration must be between 0-127."))
                                                else if snd lftName = "octave" then (* min max checking *)
                                                    if getInt rht_expr >= 0 && getInt rht_expr <= 8 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).octave <- getInt rht_expr); (locals, globals, fdecls))
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).octave <- getInt rht_expr); (locals, globals, fdecls))         
                                                        else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                    else raise (Failure ("invalid note octave: " ^ string_of_int (getInt rht_expr) ^ ". octave must be between 0-8."))
                                                else raise (Failure ("fatal error"))
                                            else raise (Failure ("cannot assign to: " ^ fst lftType)) 
                                        else raise (Failure ("cannot assign to: " ^ (fst lftType)))                                    
                                    | _ -> (* bool, note, chord, staff, part *)
                                        if lftIdType = "id" then
                                            (if snd lftType = "locals" then
                                                rht_expr, (NameMap.add (fst lftName) rht_expr locals, globals, fdecls)
                                            else if snd lftType = "globals" then
                                                rht_expr, (locals, NameMap.add (fst lftName) rht_expr globals, fdecls)
                                            else raise (Failure ("fatal error")))
                                        else raise (Failure ("cannot assign to: " ^ (fst lftType)))
                            else if lftIdType = "id" then
                                raise (Failure ("cannot assign: " ^ fst lftType ^ " = " ^ rhtType))
                            else if lftIdType = "member" then
                                raise (Failure ("cannot assign: " ^ lftRetType ^ " = " ^ rhtType))
                            else raise (Failure ("fatal error"))
    | NoExpr -> print_string ("I am nothingness\n"); Bool true, env
    | _ -> print_string ("No matching for eval\n"); Bool true, env
and exec env fname = function
        Expr(e) -> let _, env = (eval env e) in
            env
        | Return(e) -> print_string ("I am an a return statement" ^ "\n");
            let v, (locals, globals, fdecls) = (eval env e) in
                let fdecl = NameMap.find fname fdecls in
                    if (getType v) = (string_of_cbtype fdecl.rettype) then
                        raise (ReturnException(v, globals))
                    else raise (Failure ("function " ^ fdecl.fname ^ " returns: " ^ (getType v) ^ " instead of " ^ (string_of_cbtype fdecl.rettype)))
            env
        | Block(s1) -> print_string ("I am a block statement" ^ "\n");
            let (locals, globals, fdecls) = env in
                let g = call s1 locals globals fdecls fname
                in (locals, g, fdecls)
        | If(e, sl, s2) -> print_string ("I am an if statement" ^ "\n");
            let (locals, globals, fdecls) = env in
                let v, env = eval env e in
                    if getBool v = true
                    then
                        let g = call sl locals globals fdecls fname
                            in (locals, g, fdecls)
                    else
                        let env_return = exec env fname s2
                            in env_return
        | Foreach(p, a, sl) -> print_string ("I am a foreach statement" ^ "\n");
            env
        | While(e, sl) -> print_string ("I am a while statement" ^ "\n");
            let rec loop env =
                let v, env = eval env e in
                if getBool v = false then
                    let (locals, globals, fdecls) = env in
                        loop (locals, (call sl locals globals fdecls fname), fdecls)
                else
                    env
            in loop env
        | _ -> raise (Failure ("Unable to match the statment "))
(* Execute the body of a method and return an updated global map *)
and call fdecl_body locals globals fdecls fdecl_name = print_string ("---Call Running---\n");
        match fdecl_body with
            [] -> print_string ("---Method Call Complete---\n");
                globals (*When we are done return the updated globals*)
            | head::tail ->
                match head with
                    VDecl2(head) -> print_string ("---Method Processing Variable Declaration: " ^ head.varname ^ "\n");
                        call tail (NameMap.add head.varname (initIdentifier (string_of_cbtype head.vartype)) locals) globals fdecls fdecl_name
                    | FullDecl2(head) -> print_string ("---Method Processing Full Declaration: " ^ head.fvname ^ "\n");
                        let v, env = eval (locals, globals, fdecls) head.fvexpr in
                            let vType = getType v in
                                if vType = (string_of_cbtype head.fvtype)
                                    then
                                        match vType with
                                            "int" -> call tail (NameMap.add head.fvname (Int (getInt v)) locals) globals fdecls fdecl_name
                                            | "note" -> call tail (NameMap.add head.fvname (Note (getNote v)) locals) globals fdecls fdecl_name
                                            | "chord" -> call tail (NameMap.add head.fvname (Chord (getChord v)) locals) globals fdecls fdecl_name
                                            | "bool" -> call tail (NameMap.add head.fvname (Bool (getBool v)) locals) globals fdecls fdecl_name
                                            | "scale" -> call tail (NameMap.add head.fvname (Scale (getScale v)) locals) globals fdecls fdecl_name
                                            | "stanza" -> call tail (NameMap.add head.fvname (Stanza (getStanza v)) locals) globals fdecls fdecl_name
                                            | "score" -> call tail (NameMap.add head.fvname (Score (getScore v)) locals) globals fdecls fdecl_name
                                            | _ -> raise (Failure ("Unknown type: " ^ vType))
                                else
                                    raise (Failure ("LHS = " ^ (string_of_cbtype head.fvtype) ^ " <> RHS = " ^ vType))
                    | Stmt2(head) -> print_string ("---Method Processing Statement---\n");
                        let locals, globals, fdecls = (exec (locals, globals, fdecls) fdecl_name head) in
                            call tail locals globals fdecls fdecl_name
(* Executes the body of a program *)
(* and call_inner_block env list1 fname =
    let env =
        List.fold_left (fun acc x ->
            match x with
                Stmt2(x) -> print_string ("processing stmt in block");
                        let locals, globals, fdecls = acc in
                            let env_return = exec (locals, globals, fdecls) fname x
                            in env_return;
                | VDecl2(x) ->
                            print_string ("processing vdecl in block");
                            let locals, globals, fdecls = acc in
                                let env_return =
                                    (locals, (NameMap.add x.varname (initIdentifier (string_of_cbtype x.vartype)) globals), fdecls)
                                in env_return;
                | FullDecl2(x) -> print_string ("Processing Full Declaration: " ^ x.fvname ^ " in block \n");
                                    let locals, globals, fdecls = acc in
                                        let env_return =
                                            let v, acc = eval (locals, globals, fdecls) x.fvexpr in
                                                let vType = getType v in
                                                    if vType = (string_of_cbtype x.fvtype)
                                                    then
                                                        match vType with
                                                            "int" -> (locals, (NameMap.add x.fvname (Int (getInt v)) globals), fdecls);
                                                            | "note" -> (locals, (NameMap.add x.fvname (Note (getNote v)) globals), fdecls);
                                                            | "chord" -> (locals, (NameMap.add x.fvname (Chord (getChord v)) globals), fdecls);
                                                            | "bool" -> (locals, (NameMap.add x.fvname (Bool (getBool v)) globals), fdecls);
                                                            | "scale" -> (locals, (NameMap.add x.fvname (Scale (getScale v)) globals), fdecls);
                                                            | "stanza" -> (locals, (NameMap.add x.fvname (Stanza (getStanza v)) globals), fdecls);
                                                            | "score" -> (locals, (NameMap.add x.fvname (Score (getScore v)) globals), fdecls);
                                                            | _ -> raise (Failure ("Unknown type: " ^ vType))
                                                    else
                                                        raise (Failure ("LHS = " ^ (string_of_cbtype x.fvtype) ^ " <> RHS = " ^ vType))
                                        in env_return
            ) env list1;
    in env *)
and run prog env =
    let locals, globals, fdecls = env in
        if NameMap.is_empty globals then print_string ("In run, globals is empty\n") else print_string ("In run, globals in non-empty\n");
        match prog with
            [] -> print_string ("##Program Completed##\n");
                Bool true, (locals, globals, fdecls)
            | head::tail ->
                match head with
                    VDecl(head) -> print_string ("<<<Processing Variable Declaration: " ^ head.varname ^ ">>>\n");
                        run tail (locals, (NameMap.add head.varname (initIdentifier (string_of_cbtype head.vartype)) globals), fdecls)
                    | FullDecl(head) -> print_string ("<<<Processing Full Declaration: " ^ head.fvname ^ ">>>\n");
                        let v, env = eval (locals, globals, fdecls) head.fvexpr in
                            let vType = getType v in
                                if vType = (string_of_cbtype head.fvtype)
                                    then
                                        match vType with
                                            "int" -> run tail (locals, (NameMap.add head.fvname (Int (getInt v)) globals), fdecls)
                                            | "note" -> run tail (locals, (NameMap.add head.fvname (Note (getNote v)) globals), fdecls)
                                            | "chord" -> run tail (locals, (NameMap.add head.fvname (Chord (getChord v)) globals), fdecls)
                                            | "bool" -> run tail (locals, (NameMap.add head.fvname (Bool (getBool v)) globals), fdecls)
                                            | "scale" -> run tail (locals, (NameMap.add head.fvname (Scale (getScale v)) globals), fdecls)
                                            | "stanza" -> run tail (locals, (NameMap.add head.fvname (Stanza (getStanza v)) globals), fdecls)
                                            | "score" -> run tail (locals, (NameMap.add head.fvname (Score (getScore v)) globals), fdecls)
                                            | _ -> raise (Failure ("Unknown type: " ^ vType))
                                else
                                    raise (Failure ("LHS = " ^ (string_of_cbtype head.fvtype) ^ "<> RHS = " ^ vType))
                    | MDecl(head) -> print_string ("<<<Processing Method Declaration: " ^ head.fname ^ ">>>\n");
                        run tail (locals, globals, (NameMap.add head.fname head fdecls))
                    | Stmt(head) -> print_string ("<<<Processing Statement>>>\n");
                        run tail (exec (locals, globals, fdecls) "" head)

let helper prog = run prog (NameMap.empty, NameMap.empty, NameMap.empty)
