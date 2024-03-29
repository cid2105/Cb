open Ast

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
    mutable instrument : int;
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
        | Stanza(v) -> "stanza"
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
        | _ -> {stanzalist=[]; instrument=0}

let initIdentifier t =
  match t with
    "int" -> Int(0)
    | "bool" -> Bool(false)
    | "note" -> Note({pitch=128; octave=0; duration=0})
    | "chord" -> Chord({notelist=[]; chord_duration=0})
    | "scale" -> Scale({scale_notelist=[]})
    | "stanza" -> Stanza({chordlist=[]})
    | "score" -> Score({stanzalist=[]; instrument=0})
    | _ -> Bool(false)

let setOctave v a = ((getNote v).octave <- a); v
let setPitch v a = ((getNote v).pitch <- a); v
let setDuration v a = ((getNote v).duration <- a); v
let setPitch v a = ((getNote v).pitch <- a); v

let incrementNote v =
    let note_pitch = (getNote v).pitch in
    let note_octave = (getNote v).octave in
    let note_duration = (getNote v).duration in
        if note_pitch < 11 then
            Note({pitch=note_pitch+1; octave=note_octave; duration=note_duration})
        else
            if note_octave < 5 then
                Note({pitch=0; octave=note_octave+1; duration=note_duration})
            else
                raise (Failure ("Cannot increment note: already at highest pitch" ))

let decrementNote v =
    let note_pitch = (getNote v).pitch in
    let note_octave = (getNote v).octave in
    let note_duration = (getNote v).duration in
        if note_pitch > 0 then
            Note({pitch=note_pitch - 1; octave=note_octave; duration=note_duration})
        else
            if note_octave > -5 then
                Note({pitch=11; octave=note_octave-1; duration=note_duration})
            else
                raise (Failure ("Cannot decrement note: already at highest pitch" ))

let minorChord v dur =
    let scale_notes = (getScale v).scale_notelist in
        if List.length scale_notes = 8 then
            let first_note = Note (List.nth scale_notes 0) in
            let third_note = Note (List.nth scale_notes 2) in
            let fifth_note = Note (List.nth scale_notes 4) in
            let eight_note = Note (List.nth scale_notes 7) in
                let chord_notes =(List.map (fun e -> getNote e) [first_note;  (decrementNote (third_note)); fifth_note; eight_note]) in
                    Chord ({notelist=chord_notes; chord_duration = (getInt dur) })
        else
            raise (Failure ("Can only apply the minor function to scales of 8 notes" ))

let majorChord v dur =
    let scale_notes = (getScale v).scale_notelist in
        if List.length scale_notes = 8 then
            let first_note = Note (List.nth scale_notes 0) in
            let third_note = Note (List.nth scale_notes 2) in
            let fifth_note = Note (List.nth scale_notes 4) in
            let eight_note = Note (List.nth scale_notes 7) in
                let chord_notes =(List.map (fun e -> getNote e) [first_note;  (incrementNote (third_note)); fifth_note; eight_note]) in
                    Chord ({notelist=chord_notes; chord_duration= (getInt dur) })
        else
            raise (Failure ("Can only apply the minor function to scales of 8 notes" ))

 let noteMap =
     NameMap.add "R" (-1) NameMap.empty
    let noteMap = NameMap.add "C" 0 noteMap
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
let composeJava = ref ""

let rec eval env = function
    Id(name) ->
        let locals, globals, fdecls = env in
            if NameMap.mem name locals then
                (NameMap.find name locals), env
            else if NameMap.mem name globals then
                (NameMap.find name globals), env
            else raise (Failure ("undeclared identifier: " ^ name))
    | MemberAccess(vname, memname) ->
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
                    "chord_duration" -> Int (getChord v).chord_duration
                    | "notelist" -> Scale ({scale_notelist = (getChord v).notelist })
                  | _ -> raise (Failure ("invalid property of chord: " ^ memname)))
              | "score" ->
                (match memname with
                   "instrument" -> Int (getScore v).instrument
                  | _ -> raise (Failure ("invalid property of score: " ^ memname)))
              | _ -> raise (Failure ("cannot access " ^ vname ^ "." ^ memname))), env
    | IntLiteral(i) ->
        (Int i, env);
    | NoteConst(s) ->
        Int (NameMap.find s noteMap), env
    | BoolLiteral(b) ->
        (Bool b, env)
    | ChordExpr(el, e) ->
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
    | DurConst(s) ->
        if s = "whole" then Int 64, env
            else if s = "half" then Int 32, env
            else if s = "quarter" then Int 16, env
            else raise (Failure ("Duration constant unknown"))
    | NoteExpr(s,e,e1) ->
        let oct, env = eval env e in
            let octType = getType oct in
                if octType = "int" then (let dur, env = eval env e1 in
                                    let durType = getType dur in
                                        if durType = "int" then
                                        begin
                                            (Note ({pitch=(NameMap.find s noteMap); octave=(getInt oct); duration=(getInt dur)}), env);
                                        end
                                        else raise (Failure ("Duration does not evaluate to an integer")))
                else  raise (Failure ("Octave does not evaluate to an integer"))
    | BinOp(e1,o,e2) ->
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
                    if (getBool v1 || getBool v2)
                    then Bool true
                    else Bool false
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " or " ^ v2Type))
                | Eq ->
                    if v1Type = "int" then
                        if getInt v1 = getInt v2
                        then Bool true
                        else Bool false
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " is " ^ v2Type))
                | NEq ->
                    if v1Type = "int" then
                        if (getInt v1 <> getInt v2)
                        then Bool true
                        else Bool false
                    else
                        raise (Failure ("incorrect type: " ^ v1Type ^ " isnt " ^ v2Type))
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
                | _ -> raise (Failure ("Unknown binary operation"))
                (* | IDTimes -> ), env *)
            ), env
        else raise (Failure ("type mismatch: " ^ v1Type ^ " and " ^ v2Type))
    | MethodCall("print", [e]) ->
        let arg, env = eval env e in
            (if getType arg = "int" then
                print_endline (string_of_int (getInt arg))
            else if getType arg = "bool" then
                print_endline (string_of_bool (getBool arg))
            else if getType arg = "note" then
                print_endline ("(" ^ (string_of_int (getNote arg).pitch) ^ "," ^ (string_of_int (getNote arg).octave) ^ "," ^ (string_of_int (getNote arg).duration) ^ ")")
            else
                print_endline(getType arg));
            (Bool false), env
    | MethodCall("major", [e; dur]) ->
        let arg, env = eval env e in
        let arg2, env = eval env dur in
            if getType arg = "scale" && getType arg2 = "int" then
                (majorChord arg  arg2), env
            else raise (Failure ("argument of major must be a scale"))
    | MethodCall("minor", [e; dur]) ->
        let arg, env = eval env e in
        let arg2, env = eval env dur in
            if (getType arg = "scale") && (getType arg2 = "int") then
                (minorChord arg  arg2), env
            else raise (Failure ("argument of minor must be a scale"))
    | MethodCall("sharp", [e]) ->
        let arg, env = eval env e in
            if getType arg = "note" then
                incrementNote arg, env
            else raise (Failure ("argument of flat must be a note"))
    | MethodCall("flat", [e]) ->
        let arg, env = eval env e in
            if getType arg = "note" then
                decrementNote arg, env
            else raise (Failure ("argument of flat must be a note"))
    | MethodCall("randint", [e]) ->
        let v, env = eval env e in
            if getType v = "int" then
                Int(Random.int (getInt v)), env
            else raise (Failure ("argument of randint must be an integer"))
    | MethodCall("chordOfNote",[e]) ->
        let v, env = eval env e in
            if getType v = "note" then
                let dur = (getNote v).duration in
                    let tmp_list = (getNote v)::[] in
                        Chord ({notelist=tmp_list; chord_duration=dur}), env
            else raise (Failure ("argument of chordOfNote must be a note"))
    | MethodCall("rest", [e]) ->
        let v, env = eval env e in
            if getType v = "int" then
                let tmp_note = (Note ({pitch=(-1); octave=0; duration=(getInt v)})) in
                    let tmp_list = (getNote tmp_note)::[] in
                        Chord ({notelist=tmp_list; chord_duration=(getInt v)}), env
            else raise (Failure ("argument of rest must be an integer"))
    | MethodCall("prepend", [item; alist]) ->
        let arg1, env = eval env item in
            let arg2, env = eval env alist in
                if getType arg1 = "note" then
                    (if getType arg2 = "scale" then
                        (let tmp_note = arg1 in
                            let tmp_list = (List.rev ((getNote tmp_note)::(List.rev ((getScale arg2).scale_notelist)))) in
                                Scale ({scale_notelist=tmp_list}), env)
                    else if getType arg2 = "chord" then (* Returns a new chord with the note appended *)
                        (let tmp_note = arg1 in
                            let tmp_list = (List.rev ((getNote tmp_note)::(List.rev ((getChord arg2).notelist)))) in
                                Chord ({notelist=(tmp_list); chord_duration=(getChord arg2).chord_duration}), env)
                    else raise (Failure ("A note can only be prepended to a chord or scale")))
                else if getType arg1 = "chord" then
                    (if getType arg2 = "stanza" then
                        (let tmp_chord = arg1 in
                            let tmp_list = (List.rev ((getChord tmp_chord)::(List.rev ((getStanza arg2).chordlist)))) in
                                Stanza ({chordlist=tmp_list}), env)
                    else raise (Failure ("A chord can only be prepended to a stanza")))
                else if getType arg1 = "stanza" then
                    (if getType arg2 = "score" then
                        (let tmp_stanza = arg1 in
                            let tmp_list = (List.rev ((getStanza tmp_stanza)::(List.rev ((getScore arg2).stanzalist)))) in
                                Score ({stanzalist=tmp_list; instrument=(getScore arg2).instrument}), env)
                    else raise (Failure ("a stanza can only be prepended to a score")))
                else raise (Failure ("First argument for prepend must be of type note, chord, or stanza"))
    | MethodCall("append", [item; alist]) ->
        let arg1, env = eval env item in
            let arg2, env = eval env alist in
                if getType arg1 = "note" then
                    (if getType arg2 = "scale" then
                        (let tmp_note = arg1 in
                            let tmp_list = (getNote tmp_note)::((getScale arg2).scale_notelist) in
                                Scale ({scale_notelist=tmp_list}), env)
                    else if getType arg2 = "chord" then (* Returns a new chord with the note appended *)
                        (let tmp_note = arg1 in
                            let tmp_list = (getNote tmp_note)::((getChord arg2).notelist) in
                                Chord ({notelist=(tmp_list); chord_duration=(getChord arg2).chord_duration}), env)
                    else raise (Failure ("A note can only be appended to a chord or scale")))
                else if getType arg1 = "chord" then
                    (if getType arg2 = "stanza" then
                        (let tmp_chord = arg1 in
                            let tmp_list = (getChord tmp_chord)::((getStanza arg2).chordlist) in
                                Stanza ({chordlist=tmp_list}), env)
                    else raise (Failure ("A chord can only be appended to a stanza")))
                else if getType arg1 = "stanza" then
                    (if getType arg2 = "score" then
                        (let tmp_stanza = arg1 in
                            let tmp_list = (getStanza tmp_stanza)::((getScore arg2).stanzalist) in
                                Score ({stanzalist=tmp_list; instrument=(getScore arg2).instrument}), env)
                    else raise (Failure ("a stanza can only be appended to a score")))
                else raise (Failure ("First argument for append must be of type note, chord, or stanza"))
    | MethodCall("concat", [list1; list2]) ->
        let arg1, env = eval env list1 in
            let arg2, env = eval env list2 in
                if getType arg1 = getType arg2 then
                    (if getType arg1 = "stanza" then
                        (let tmp_chordlist = ((getStanza arg2).chordlist) @ ((getStanza arg1).chordlist) in
                            Stanza ({chordlist=tmp_chordlist}), env)
                    else if getType arg1 = "scale" then
                        (let tmp_notelist = ((getScale arg2).scale_notelist) @ ((getScale arg1).scale_notelist) in
                            Scale ({scale_notelist=tmp_notelist}), env)
                    else if getType arg1 = "score" then
                        (let tmp_stanzalist = ((getScore arg2).stanzalist) @ ((getScore arg1).stanzalist) in
                            Score ({stanzalist=tmp_stanzalist; instrument=(getScore arg1).instrument}), env)
                    else raise (Failure ("concat works only on stanzas and scores")))
                else raise (Failure ("Both arguments to concat must be of the same type"))
    | MethodCall("repeat", [e; n]) -> (*Takes the argument and returns its container type with arg repeated n times*)
        let arg1, env = eval env e in
            let arg2, env = eval env n in
                if getType arg2 = "int" then
                    (if ((getInt arg2) > 0) then
                        (if getType arg1 = "note" then
                            (let rec repeater alist times =
                                (if times = 0 then
                                    (alist)
                                else (repeater (Scale ({scale_notelist=((getNote arg1)::((getScale alist).scale_notelist))})) (times-1)))
                            in (repeater (Scale ({scale_notelist=[]})) (getInt arg2)), env)
                        else if getType arg1 = "chord" then
                            (let rec repeater alist times =
                                (if times = 0 then
                                    (alist)
                                else (repeater (Stanza ({chordlist=((getChord arg1)::((getStanza alist).chordlist))})) (times-1)))
                            in (repeater (Stanza ({chordlist=[]})) (getInt arg2)), env)
                        else if getType arg1 = "stanza" then
                            (let rec repeater alist times =
                                (if times = 0 then
                                    (alist)
                                else (repeater (Score ({stanzalist=((getStanza arg1)::((getScore alist).stanzalist)); instrument=0})) (times-1)))
                            in (repeater (Score ({stanzalist=[]; instrument=0})) (getInt arg2)), env)
                        else if getType arg1 = "score" then
                            (let rec repeater alist times =
                                (if times = 0 then
                                    (alist)
                                else (repeater (Score ({stanzalist=(((getScore arg1).stanzalist) @ ((getScore alist).stanzalist)); instrument=(getScore arg1).instrument})) (times-1)))
                            in (repeater (Score ({stanzalist=[]; instrument=(getScore arg1).instrument})) (getInt arg2)), env)
                        else raise (Failure ("The first argument must be a note, chord, stanza, or score")))
                    else raise (Failure ("The number of times to repeat must be 1 or greater, you asked for " ^ (string_of_int (getInt arg2)))))
                else raise (Failure ("The second argument to repeat must be an integer number of times to repeat"))
    | MethodCall("compose", e) ->  (* Writes the specified part to a java file to be written into midi *)
        ignore(
            if ( (List.length e) > 16) then
                raise (Failure ("only up to 16 scores can be composed at once"));
        );

        let score_names = (List.map ( fun e1 ->  match e1 with
                                        Id(i) -> i;
                                        | _ ->  raise (Failure ("compose takes an identifier as input"))) (List.rev e);
                            );
        in
        let actuals, env = (* make sure all ids are known *)
            List.fold_left (fun (al, env) actual ->
                                let v, env = ((eval env) actual) in (v :: al), env
                            ) ([], env) e;
        in
        ignore (
            List.map (fun act ->  (* ids need to be scores *)
                        match (getType act) with
                            "score" -> act; (* print_string ("score"^ "\n\n"); *)
                            | _ -> raise (Failure ("compose takes a score only"));

                        ) (List.rev actuals);
        );
        composeJava :=
            "\tArrayList<score> data = new ArrayList<score>();\n"^

            String.concat "\n" (List.map (fun scor ->

                                            "\tdata.add("^ scor ^");"

                                        ) (List.rev score_names);)

                ^ "\n\tthis.compose(data);\n";
        Bool true, env

    | MethodCall(name, el) ->
        let locals, globals, fdecls = env in
            let fdecl = try (NameMap.find name fdecls)
                        with Not_found -> raise (Failure ("Undefined function: " ^ name))
            in
                let actuals, env = List.fold_left
                    (fun (al, env) actual ->
                        let v, env = ((eval env) actual) in (v :: al), env
                    ) ([], env) el
                in
                    let l1 =
                        try List.fold_left2 (fun locals formal actual ->
                                                if (getType actual) = (string_of_cbtype formal.paramtype) then
                                                    (NameMap.add formal.paramname actual locals)
                                                else
                                                    raise (Failure ("Wrong parameter type in method call to " ^ fdecl.fname))
                                            ) NameMap.empty fdecl.formals (List.rev actuals)
                        with Invalid_argument(_) -> raise (Failure ("wrong number of arguments to: " ^ fdecl.fname))
                    in
                    begin
                        try
                            let l, g = (call fdecl.body l1 globals fdecls name) in
                                Bool false, (l, g, fdecls) (* This gets hit if you never see a return statement *)
                       with ReturnException(v, g) -> v, (l1, globals, fdecls) (* This gets hit if you hit a return statement *)
                   end
    | UnaryOp(uo,e) ->
        let v, env = eval env e in
        let vType = getType v in
        if ( vType = "note" or vType = "chord" ) then
            (match uo with (* Only accept notes for now *)
                Raise ->
                    if vType = "note" then
                        setOctave v ((getNote v).octave + 1)
                    else
                        raise (Failure ("cannot raise: " ^ vType))
                | Lower ->
                    if vType = "note" then
                        setOctave v ((getNote v).octave - 1)
                    else
                        raise (Failure ("cannot lower: " ^ vType))
            ), env
        else raise (Failure ("type mismatch: " ^ vType ^ " is not suitable, must be a note or chord"))
    | ListExpr(el) -> (*  el is elment list *)
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
                                (Score ({stanzalist = stanza_list; instrument = 0}), env);
                    | _ -> raise (Failure ("List expression must only contain notes or chords or stanzas"))
            end
    | Assign(toE, fromE) ->
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
                                        (* MEMBER METHODS *)
                                        else if lftIdType = "member" then
                                            (* NOTE MEMBER METHODS *)
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
                                                    if snd lftType = "locals" then
                                                        rht_expr, (((getNote (NameMap.find (fst lftName) locals)).duration <- getInt rht_expr); (locals, globals, fdecls))
                                                    else if snd lftType = "globals" then
                                                        rht_expr, (((getNote (NameMap.find (fst lftName) globals)).duration <- getInt rht_expr); (locals, globals, fdecls))
                                                    else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                else if snd lftName = "octave" then (* min max checking *)
                                                    if getInt rht_expr >= -5 && getInt rht_expr <= 5 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).octave <- getInt rht_expr); (locals, globals, fdecls))
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).octave <- getInt rht_expr); (locals, globals, fdecls))
                                                        else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                    else raise (Failure ("invalid note octave: " ^ string_of_int (getInt rht_expr) ^ ". octave must be between -5-5."))
                                                else raise (Failure ("fatal error"))
                                            else if fst lftType = "chord" then
                                                if snd lftName = "duration" then (* min max checking *)
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).duration <- getInt rht_expr); (locals, globals, fdecls))
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).duration <- getInt rht_expr); (locals, globals, fdecls))
                                                        else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                else raise (Failure ("fatal error"))
                                            else if fst lftType = "score" then
                                                if snd lftName = "instrument" then
                                                    if getInt rht_expr >= 0 && getInt rht_expr <= 127 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getScore (NameMap.find (fst lftName) locals)).instrument <- getInt rht_expr); (locals, globals, fdecls))
                                                    else if snd lftType = "globals" then
                                                        rht_expr, (((getScore (NameMap.find (fst lftName) globals)).instrument <- getInt rht_expr); (locals, globals, fdecls))
                                                    else raise (Failure ("fatal error"))
                                                else raise (Failure ("invalid score instrument: " ^ string_of_int (getInt rht_expr) ^ ". instrument must be between 0-127."))
                                            else raise (Failure ("fatal error"))
                                            else raise (Failure ("cannot assign to: " ^ fst lftType))
                                        else raise (Failure ("cannot assign to: " ^ (fst lftType)))
                                    | "scale" ->
                                        if lftIdType = "id" then
                                            (if snd lftType = "locals" then
                                                rht_expr, (NameMap.add (fst lftName) rht_expr locals, globals, fdecls)
                                            else if snd lftType = "globals" then
                                                rht_expr, (locals, NameMap.add (fst lftName) rht_expr globals, fdecls)
                                            else raise (Failure ("fatal error")))
                                        (* MEMBER METHODS *)
                                        else if lftIdType = "member" then
                                            (* NOTE MEMBER METHODS *)
                                            if fst lftType = "chord" then
                                                if snd lftName = "notelist" then
                                                    if snd lftType = "locals" then
                                                        rht_expr, (((getChord (NameMap.find (fst lftName) locals)).notelist <- (getScale rht_expr).scale_notelist); (locals, globals, fdecls))
                                                    else if snd lftType = "globals" then
                                                        rht_expr, (((getChord (NameMap.find (fst lftName) globals)).notelist <- (getScale rht_expr).scale_notelist); (locals, globals, fdecls))
                                                    else raise (Failure ("undeclared identifier: " ^ fst lftName))
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
    | NoExpr -> Bool true, env
and exec env fname = function
        Expr(e) -> let _, env = (eval env e) in
            env
        | Return(e) ->
            let v, (locals, globals, fdecls) = (eval env e) in
                let fdecl = NameMap.find fname fdecls in
                    if (getType v) = (string_of_cbtype fdecl.rettype) then
                        raise (ReturnException(v, globals))
                    else raise (Failure ("function " ^ fdecl.fname ^ " returns: " ^ (getType v) ^ " instead of " ^ (string_of_cbtype fdecl.rettype)))

        | Block(s1) ->
            let (locals, globals, fdecls) = env in
                let l, g = call s1 locals globals fdecls fname
                in (l, g, fdecls)
        | If(e, ibl, s) ->
            let (locals, globals, fdecls) = env in
                let v, env = eval env e in
                    if getBool v = true
                    then
                        let l, g = call (List.rev ibl) locals globals fdecls fname
                            in (l, g, fdecls)
                    else
                        let env_return = exec env fname s
                            in env_return
        | Foreach(par_decl, list_name, sl) ->
            let locals, globals, fdecls = env in (* env *)
                let list1 = (*check for var existence in locals *)
                    if NameMap.mem list_name locals then (*check if list_name is in locals *)
                        NameMap.find list_name locals (*let list equal to the variable found in locals map*)
                    else
                        if NameMap.mem list_name globals then (*let list equal to the variable found in globals map*)
                            NameMap.find list_name globals
                        else
                            raise (Failure ("list variable undeclared"))
                in let vType = getType list1 in
                begin
                    match vType with
                        "chord" ->
                            (*notes*)
                            if (string_of_cbtype par_decl.paramtype) = "note" then
                                let llist = (List.rev (getChord list1).notelist) in
                                    List.fold_left (fun acc x -> let (l1,g1,f1) = acc in
                                        let (l, g) = (call (List.rev sl) (NameMap.add par_decl.paramname (Note x) l1) g1 f1 fname) in
                                                        (l, g, fdecls)
                                                ) (locals, globals, fdecls) llist
                            else
                                raise (Failure ("failure of type matching with chord list"))
                        | "scale" ->
                            (*notes*)
                            if (string_of_cbtype par_decl.paramtype) = "note" then
                                let llist = (List.rev (getScale list1).scale_notelist) in
                                    List.fold_left (fun acc x -> let (l1,g1,f1) = acc in
                                        let (l, g) = (call (List.rev sl) (NameMap.add par_decl.paramname (Note x) l1) g1 f1 fname) in
                                                        (l, g, fdecls)
                                        ) (locals, globals, fdecls) llist
                            else
                                raise (Failure ("failure of type matching with scale list"))
                        | "stanza" ->
                            (*chords*)
                            if (string_of_cbtype par_decl.paramtype) = "chord" then
                                let llist = (List.rev (getStanza list1).chordlist) in
                                    List.fold_left (fun acc x -> let (l1,g1,f1) = acc in
                                        let (l, g) = (call (List.rev sl) (NameMap.add par_decl.paramname (Chord x) l1) g1 f1 fname) in
                                                        (l, g, fdecls)
                                        ) (locals, globals, fdecls) llist
                            else
                                raise (Failure ("failure of type matching with stanza list"))
                        | "score" ->
                            (*stanzas*)
                            if (string_of_cbtype par_decl.paramtype) = "stanza" then
                                let llist = (List.rev (getScore list1).stanzalist) in
                                    List.fold_left (fun acc x -> let (l1,g1,f1) = acc in
                                        let (l, g) = (call (List.rev sl) (NameMap.add par_decl.paramname (Stanza x) l1) g1 f1 fname) in
                                                        (l, g, fdecls)
                                        ) (locals, globals, fdecls) llist
                            else
                                raise (Failure ("failure of type matching with score list"))
                        | _ ->
                            raise (Failure ("undesired list type for for_each loop"))
                end
        | While(e, sl) ->
            let rec loop env =
                let v, env = eval env e in
                if getBool v = true then
                    let (locals, globals, fdecls) = env in
                        let l, g = call sl locals globals fdecls fname in
                            loop (l, g, fdecls)
                else
                    env
            in loop env
        | _ -> raise (Failure ("Unable to match the statment "))
(* Execute the body of a method and return an updated global map *)
and call fdecl_body locals globals fdecls fdecl_name =
        match fdecl_body with
            [] ->
                (locals, globals) (*When we are done return the updated globals*)
            | head::tail ->
                match head with
                    VDecl2(head) ->
                        call tail (NameMap.add head.varname (initIdentifier (string_of_cbtype head.vartype)) locals) globals fdecls fdecl_name
                    | FullDecl2(head) ->
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
                    | Stmt2(head) ->
                        let locals, globals, fdecls = (exec (locals, globals, fdecls) fdecl_name head) in
                            call tail locals globals fdecls fdecl_name
and translate prog env =
    let locals, globals, fdecls = env in
        match prog with
            [] -> (* everything went well, write the java file and quit *)
                Bool true, (locals, globals, fdecls)
            | head::tail ->
                match head with
                    VDecl(head) ->
                        translate tail (locals, (NameMap.add head.varname (initIdentifier (string_of_cbtype head.vartype)) globals), fdecls)
                    | FullDecl(head) ->
                        let v, env = eval (locals, globals, fdecls) head.fvexpr in
                            let vType = getType v in
                                if vType = (string_of_cbtype head.fvtype)
                                    then
                                        match vType with
                                            "int" -> translate tail (locals, (NameMap.add head.fvname (Int (getInt v)) globals), fdecls)
                                            | "note" -> translate tail (locals, (NameMap.add head.fvname (Note (getNote v)) globals), fdecls)
                                            | "chord" -> translate tail (locals, (NameMap.add head.fvname (Chord (getChord v)) globals), fdecls)
                                            | "bool" -> translate tail (locals, (NameMap.add head.fvname (Bool (getBool v)) globals), fdecls)
                                            | "scale" -> translate tail (locals, (NameMap.add head.fvname (Scale (getScale v)) globals), fdecls)
                                            | "stanza" -> translate tail (locals, (NameMap.add head.fvname (Stanza (getStanza v)) globals), fdecls)
                                            | "score" -> translate tail (locals, (NameMap.add head.fvname (Score (getScore v)) globals), fdecls)
                                            | _ -> raise (Failure ("Unknown type: " ^ vType))
                                else
                                    raise (Failure ("LHS = " ^ (string_of_cbtype head.fvtype) ^ "<> RHS = " ^ vType))
                    | MDecl(head) ->
                        translate tail (locals, globals, (NameMap.add head.fname head fdecls))
                    | Stmt(head) ->
                        translate tail (exec (locals, globals, fdecls) "" head)

let helper prog = translate prog (NameMap.empty, NameMap.empty, NameMap.empty)
