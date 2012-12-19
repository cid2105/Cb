open Ast
open Printf
open String

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
    "int" -> Int(1)
    | "bool" -> Bool(false)
    | "note" -> Note({pitch=128; octave=0; duration=0})
    | "chord" -> Chord({notelist=[]; chord_duration=0})
    | "scale" -> Scale({scale_notelist=[]})
    | "stanza" -> Stanza({chordlist=[]})
    | "score" -> Score({stanzalist=[]; instrument=0})
    | _ -> Bool(false)

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
let methJava = ref ""

let methJava = ref ""
let globalJava = ref ""
let mainJava = ref ""

let import_decl =
"import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import javax.sound.midi.InvalidMidiDataException;
import javax.sound.midi.MidiEvent;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Sequence;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.Track;"

let class_start =
"
class note {

    public int pitch;
    public int octave;
    public int duration;

    public note() {
        pitch = 0;
        octave = 0;
        duration = 0;
    }

    public note(int p, int o, int d) {
        pitch = p;
        octave = o;
        duration = d;
    }

    public void inc_oct(){
        this.octave++;
    }
    public void dec_oct(){
        this.octave--;
    }

    public boolean isValid() {
        return -1 <= pitch && 11 >= pitch && -5 <= octave && 5 <= octave;
    }

    public String toString()  {
        return \"(\" + pitch + \",\" + octave + \",\" + duration + \")\";
    }

    public chord toChord() {
        ArrayList<note> tmp_list = new ArrayList<note>(1);
        tmp_list.add(new note(pitch, octave, duration));
        return new chord(tmp_list, duration);
    }

    public note deepCopy() {
        return new note(this.pitch, this.octave, this.duration);
    }
}

class chord {

    public ArrayList<note> notelist;
    public int chord_duration;

    public chord() {
        notelist = new ArrayList<note>();
        chord_duration = 0;
    }

    public chord(ArrayList<note> nl, int cd) {
        notelist = nl;
        chord_duration = cd;
    }

    public String toString() {
        String s = \"([\";
        for(note n : notelist) {
            s += n.toString();
        }
        return s + \"], \" + chord_duration + \")\";
    }

    private ArrayList<note> nlCopy() {
        ArrayList<note> tmp = new ArrayList<note>(notelist.size());
        for(note n : notelist) {
            tmp.add(n.deepCopy());
        }
        return tmp;
    }

    public chord deepCopy() {
        return new chord(nlCopy(), this.chord_duration);
    }
}

class scale {

    public ArrayList<note> scale_notelist;

    public scale() {
        scale_notelist = new ArrayList<note>();
    }

    public scale(ArrayList<note> snl) {
        scale_notelist = snl;
    }

    public String toString() {
        String s = \"[\";
        for(note n : scale_notelist) {
            s += n.toString();
        }
        return s + \"]\";
    }

    public scale deepCopy() {
        ArrayList<note> tmp = new ArrayList<note>(scale_notelist.size());
        for(note n : scale_notelist) {
            tmp.add(n.deepCopy());
        }
        return new scale(tmp);
    }
}

class stanza {

    public ArrayList<chord> chordlist;

    public stanza() {
        chordlist = new ArrayList<chord>();
    }

    public stanza(ArrayList<chord> cl) {
        chordlist = cl;
    }

    public String toString() {
        String s = \"[\";
        for(chord c : chordlist) {
            s += c.toString();
        }
        return s + \"]\";
    }

    public stanza deepCopy() {
        ArrayList<chord> tmp = new ArrayList<chord>(chordlist.size());
        for(chord c : chordlist) {
            tmp.add(c.deepCopy());
        }
        return new stanza(tmp);
    }
}

class score {

    public ArrayList<stanza> stanzalist;
    public int instrument;

    public score() {
        stanzalist = new ArrayList<stanza>();
        instrument = 0;
    }
    public score(ArrayList<stanza> sl) {
        stanzalist = sl;
        instrument = 0;
    }

    public score(ArrayList<stanza> sl, int i) {
        stanzalist = sl;
        instrument = i;
    }

    public String toString() {
        String s = \"([\";
        for(stanza st : stanzalist) {
            s += st.toString();
        }
        return s + \"], instrument=\" + instrument + \")\";
    }

    public score deepCopy() {
        ArrayList<stanza> tmp = new ArrayList<stanza>(stanzalist.size());
        for(stanza s : stanzalist) {
            tmp.add(s.deepCopy());
        }
        return new score(tmp, this.instrument);
    }
}

public class Cb {
    /**
     * ********************compose helper functions****************************
     * source: http://www.penguinpeepshow.com/CSV2MIDI.php
     * helper function for mapping values of pitch and octave to range 0 - 127
     */
    static long map(long x, long in_min, long in_max, long out_min, long out_max) {
        return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
    }
    /*
     * note representation: tick, duration, pitch, volume/loudness/velocity
     * turns note on
     */

    private static MidiEvent createNoteOnEvent(int nKey, long lTick, int channel, int velocity) {
        return createNoteEvent(ShortMessage.NOTE_ON, nKey, velocity, lTick, channel);
    }

    /*
     * turns note off
     */
    private static MidiEvent createNoteOffEvent(int nKey, long lTick, int channel) {
        return createNoteEvent(ShortMessage.NOTE_OFF, nKey, 0, lTick, channel);  //set note to 0 velocity
    }

    /*
     * turns note on or off
     */
    private static MidiEvent createNoteEvent(int nCommand, int nKey, int nVelocity, long lTick, int channel) {
        ShortMessage message = new ShortMessage();
        try {
            message.setMessage(nCommand, channel, nKey, nVelocity);
        } catch (InvalidMidiDataException e) {
            e.printStackTrace();
            System.exit(1);
        }
        MidiEvent event = new MidiEvent(message, lTick);
        return event;
    }
    /*
     * Here are all of the built in methods that we have and while we
     * are translating we need to make sure you do not overwrite any of these
     * bad boys unless you give it different args (or maybe we can just let
     * java take care of that, I am not fully certain)
     * Note: this score list has to hold at most 16 stanzas
     */
    public static void compose(ArrayList<score> data) throws InvalidMidiDataException {
        int nChannels = data.size();
        Sequence sequence = null;


        for (int i = 0; i < data.size(); i++) {
            System.out.println(data.get(i));
        }


        //***** Read in timing resolution and instruments *****
        int timingRes = 4, instrument[] = new int[nChannels];

        //read in instrument numbers
        for (int inst = 0; inst < data.size(); inst++) {
            //check if this is an integer
            instrument[inst] = data.get(inst).instrument; //this is a number, it has to be an intrument
            System.out.println(\"Instrument set to \" + instrument[inst] + \" on channel \" + inst);
        }

        //***** Initialize Sequencer *****
        try {
            sequence = new Sequence(Sequence.PPQ, timingRes);   //initialize sequencer with timingRes
        } catch (InvalidMidiDataException e) {
            e.printStackTrace();
            System.exit(1);
        }


        //***** Create tracks and notes *****
        /* Track objects cannot be created by invoking their constructor
         directly. Instead, the Sequence object does the job. So we
         obtain the Track there. This links the Track to the Sequence
         automatically.
         */
        Track track[] = new Track[nChannels];
        for (int i = 0; i < nChannels; i++) {
            track[i] = sequence.createTrack();                    //create tracks

            ShortMessage sm = new ShortMessage();
            sm.setMessage(ShortMessage.PROGRAM_CHANGE, i, instrument[i], 0);  //put in instrument[i] in this track
            track[i].add(new MidiEvent(sm, 0));
        }

        int nt = 0,
                tick = 0,
                duration = 5,
                velocity = 100; // this is the volume of the sound

        for (int channel = 0; channel < data.size(); channel++) {
            //populating the ith track
            ArrayList<stanza> stan = data.get(channel).stanzalist;
            for (int tr = 0; tr < stan.size(); tr++) {

                ArrayList<chord> chl = stan.get(tr).chordlist;
                for (int cl = 0; cl < chl.size(); cl++) { //chord list

                    ArrayList<note> tnote = chl.get(cl).notelist;
                    for (int nti = 0; nti < tnote.size(); nti++) { //note list

                        duration = (int) (chl.get(cl).chord_duration / 4);  //second number is duration
                        System.out.println(\"dur:-\" + duration + \"> \\n \" );
                        // octave (-5 to 5); pitch (0 to 11)
                        nt = (int) map((long) (tnote.get(nti).pitch + tnote.get(nti).octave), -5, 16, 0, 127); //this is the pitch representation; middle c = c_4 = 60
                        velocity = 127;  //velocity can not be changed for now

                        if (tnote.get(nti).pitch < 0) { // a rest is received -- any negative note is rest

                            nt = 0;
                            track[channel].add(createNoteOffEvent(nt, tick, channel));              //add note to this track

                        } else {
                            track[channel].add(createNoteOnEvent(nt, tick, channel, velocity));             //add note to this track
                        }
                        tick = tick + duration;  //first number is tick
                        track[channel].add(createNoteOffEvent(nt, tick + duration, channel));
                    }
                    tick = tick + duration;
                     System.out.println(\"tick:-\" + tick + \"> \\n \" + duration);
                }
            }
            tick = 0;
        }


        // Print track information
        // Print track information

        System.out.println();

        if (track != null) {

            for (int i = 0; i < track.length; i++) {

                System.out.println(\"Track \" + i + \":\");



                for (int j = 0; j < track[i].size(); j++) {

                    MidiEvent event = track[i].get(j);

                    // System.out.println(\" tick \" + event.getTick() + \", \" + MessageInfo.toString(event.getMessage()));

                }

            }

        }


        /* Now we just save the Sequence to the file we specified.
         The '0' (second parameter) means saving as SMF type 0.
         (type 1 is for multiple tracks).
         */
        try {
            MidiSystem.write(sequence, 1, new File(\"out.mid\"));
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    // assumes it is passed a minor scale
    public static chord major(scale s) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception(\"To convert a scale into a major chord, you must pass a scale of at least five notes\");

        int duration = s.scale_notelist.get(0).duration;

        return new chord(notes, duration);
    }

    public static chord major(scale s, int duration) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception(\"To convert a scale into a major chord, you must pass a scale of at least five notes\");
        return new chord(notes, duration);
    }

    public static chord minor(scale s) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception(\"To convert a scale into a minor chord, you must pass a scale of at least five notes\");
        int duration = s.scale_notelist.get(0).duration;
        return new chord(notes, duration);
    }

    public static chord minor(scale s, int duration) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception(\"To convert a scale into a minor chord, you must pass a scale of at least five notes\");
        return new chord(notes, duration);
    }


    public static note sharp(note n) throws Exception {
        note sharped = new note();
        if(n.pitch == 11)
            sharped = new note(0, n.octave+1, n.duration);
        else
            sharped = new note(n.pitch + 1, n.octave, n.duration);
        if(sharped.isValid())
            return sharped;
        else
            throw new Exception(\"To convert a scale into a minor chord, you must pass a scale of at least five notes\");
    }

    public static note flat(note n) {
        return new note();
    }

    /**
     * Return a random integer between 0 and i
     * @param i The maximum value for the integer
     * @return A random integer between 0 and i
     */
    public static int randint(int i) {
        return (int)(Math.random()*i);
    }

    public static chord chordOfNote(note n) {
        return n.toChord();
    }

    public static chord rest(int d) {
        ArrayList<note> temp = new ArrayList<note>(1);
        temp.add(new note(-1, 0, d));
        return new chord(temp, d);
    }

    public static chord prepend(note n, chord c) {
        chord tmp = c.deepCopy();
        tmp.notelist.add(0, n.deepCopy());
        return tmp;
    }

    public static scale prepend(note n, scale s) {
        scale tmp = s.deepCopy();
        tmp.scale_notelist.add(0, n.deepCopy());
        return tmp;
    }

    public static stanza prepend(chord c, stanza s) {
        stanza tmp = s.deepCopy();
        tmp.chordlist.add(0, c.deepCopy());
        return tmp;
    }

    public static score prepend(stanza st, score sc) {
        score tmp = sc.deepCopy();
        tmp.stanzalist.add(0, st.deepCopy());
        return tmp;
    }

    public static chord append(note n, chord c) {
        chord tmp = c.deepCopy();
        tmp.notelist.add(n.deepCopy());
        return tmp;
    }

    public static scale append(note n, scale s) {
        scale tmp = s.deepCopy();
        tmp.scale_notelist.add(n.deepCopy());
        return tmp;
    }

    public static stanza append(chord c, stanza s) {
        stanza tmp = s.deepCopy();
        tmp.chordlist.add(c.deepCopy());
        return tmp;
    }

    public static score append(stanza st, score sc) {
        score tmp = sc.deepCopy();
        tmp.stanzalist.add(st.deepCopy());
        return tmp;
    }

    public static scale concat(scale s1, scale s2) {
        scale tmp = s1.deepCopy();
        for(note n : s2.scale_notelist) {
            tmp.scale_notelist.add(n.deepCopy());
        }
        return tmp;
    }

    public static stanza concat(stanza s1, stanza s2) {
        stanza tmp = s1.deepCopy();
        for(chord c : s2.chordlist) {
            tmp.chordlist.add(c.deepCopy());
        }
        return tmp;
    }

    public static score concat(score s1, score s2) {
        score tmp = s1.deepCopy();
        for(stanza s : s2.stanzalist) {
            tmp.stanzalist.add(s.deepCopy());
        }
        return tmp;
    }

    public static scale repeat(note n, int i) throws Exception {
        scale tmp = new scale();
        if (i < 1) {
            throw new Exception(\"repeat function takes an integer that must be 1 or greater\");
        }
        for(int j = 0; j < i; j++) {
            tmp.scale_notelist.add(n.deepCopy());
        }
        return new scale();
    }

    public static stanza repeat(chord c, int i) throws Exception {
        stanza tmp = new stanza();
        if (i < 1) {
            throw new Exception(\"repeat function takes an integer that must be 1 or greater\");
        }
        for(int j = 0; j < i; j++) {
            tmp.chordlist.add(c.deepCopy());
        }
        return new stanza();
    }

    public static score repeat(stanza s, int i) throws Exception{
        score tmp = new score();
        if (i < 1) {
            throw new Exception(\"repeat function takes an integer that must be 1 or greater\");
        }
        tmp.instrument = 0;
        for(int j = 0; j < i; j++) {
            tmp.stanzalist.add(s.deepCopy());
        }
        return tmp;
    }

    public static score repeat(score s, int i) throws Exception{
        score tmp = new score();
        if (i < 1) {
            throw new Exception(\"repeat function takes an integer that must be 1 or greater\");
        }
        for(int j = 0; j < i; j++) {
            for(stanza st : s.stanzalist) {
                tmp.stanzalist.add(st.deepCopy());
            }
        }
        tmp.instrument = s.instrument;
        return tmp;
    }

"

let main_start =
"
    public static void main(String[] args) throws Exception { Cb runner = new Cb(); runner.run(); }
"

let run_start = "public void run() throws Exception {"

let block_start = " {
"

let block_end = " }
"

let run_end =
"
    }
"

let class_end =
"
}
"

let rec eval env = function
    Id(name) -> (print_string ("Evaluating ID: " ^ name ^ "\n"));
        let locals, globals, fdecls = env in
            if NameMap.mem name locals then
                (NameMap.find name locals), env, name
            else if NameMap.mem name globals then
                (NameMap.find name globals), env, name
            else raise (Failure ("undeclared identifier: " ^ name))
    | MemberAccess(vname, memname) -> (print_string ("Evaluating Member Access: " ^ vname ^ " Member: " ^ memname ^ "\n"));
        let v, env, asJava = eval env (Id vname) in
            let vType = getType v in
            (match vType with
              | "note" ->
                (match memname with
                  "pitch" -> (initIdentifier "int")
                  | "octave" -> (initIdentifier "int")
                  | "duration" -> (initIdentifier "int")
                  | _ -> raise (Failure ("invalid property of note: " ^ memname)))
              | "chord" ->
                (match memname with
                    "chord_duration" -> (initIdentifier "int")
                    | "notelist" -> (initIdentifier "scale")
                  | _ -> raise (Failure ("invalid property of chord: " ^ memname)))
              | "score" ->
                (match memname with
                   "instrument" -> (initIdentifier "int")
                  | _ -> raise (Failure ("invalid property of score: " ^ memname)))
              | "scale" ->
                (match memname with
                  "scale_notelist" -> (initIdentifier "scale")
                  | _ -> raise (Failure ("invalid property of score: " ^ memname)))
              | _ -> raise (Failure ("cannot access " ^ vname ^ "." ^ memname))), env, (asJava ^ "." ^ memname ^ " ")
    | IntLiteral(i) -> (print_string ("Evaluating an intlit: " ^ (string_of_int i) ^ "\n"));
        (initIdentifier "int"), env, (string_of_int i)
    | NoteConst(s) -> (print_string ("Evaluating a noteConst: " ^ s ^ "\n"));
        (initIdentifier "int"), env, (string_of_int (NameMap.find s noteMap))
    | BoolLiteral(b) -> (print_string ("Evaluating a bool literal: " ^ (string_of_bool b) ^ "\n"));
        (initIdentifier "bool"), env, (string_of_bool b)
    | ChordExpr(el, e) -> (print_string ("Evaluating a chordexpr\n"));
        let note_list = List.map (fun (note_elem) ->
            (let chord_elem, env, asJava = eval env note_elem in
                let vType = (getType chord_elem) in
                    if ( vType = "note") then (getNote (chord_elem))
                    else raise (Failure ("Chord must be composed of notes "))
            )) el in
                let javaStrList = List.map (fun (note_elem) ->
                    (let chord_elem, env, asJava = eval env note_elem in
                        let vType = (getType chord_elem) in
                            if ( vType = "note") then ("add(" ^ asJava ^ ");")
                            else raise (Failure ("Chord must be composed of notes "))
                    )) el in
                let chordAsJava = String.concat "\n" javaStrList in
                    let dur, env, durAsJava = eval env e in
                        let durType = getType dur in
                            if durType = "int" then
                                (Chord ({notelist=note_list; chord_duration=
                                    (getInt dur)}),
                                    env,
                                    ("new chord(new ArrayList<note>() {{\n" ^ chordAsJava ^ "}}, " ^ durAsJava ^ ")")
                                )
                            else raise (Failure ("Duration does not evaluate to an integer"))
    | DurConst(s) -> (print_string ("Evaluating a durConst: " ^ s ^ "\n"));
        if s = "whole" then Int 64, env, "64"
            else if s = "half" then Int 32, env, "32"
            else if s = "quarter" then Int 16, env, "16"
            else raise (Failure ("Duration constant unknown"))
    | NoteExpr(s,e,e1) -> (print_string ("Evaluating a NoteExpr\n"));
        let oct, env, octAsJava = eval env e in
            let octType = getType oct in

                if octType = "int" then (let dur, env, durAsJava = eval env e1 in
                                    let durType = getType dur in
                                        if durType = "int" then
                                        begin
                                            (Note ({pitch=(NameMap.find s noteMap); octave=(getInt oct); duration=(getInt dur)}),
                                            env,
                                            (" new note(" ^ (string_of_int (NameMap.find s noteMap)) ^ "," ^ octAsJava ^ "," ^ durAsJava ^ ")"));
                                        end
                                        else raise (Failure ("Duration does not evaluate to an integer")))

                else  raise (Failure ("Octave does not evaluate to an integer"))
    | BinOp(e1,o,e2) -> (print_string ("Evaluating a binop\n"));
        let v1, env, v1AsJava = eval env e1 in
        let v2, env, v2AsJava = eval env e2 in
        let v1Type = getType v1 in
        let v2Type = getType v2 in
        (* Two variables have to be of the same type for binop *)
        if v1Type = v2Type then
            (match o with (* Only accept ints for now *)
                Add ->
                    if v1Type = "int" then
                         (initIdentifier "int")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " + " ^ v2Type))
                | Sub ->
                    if v1Type = "int" then
                        (initIdentifier "int")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " - " ^ v2Type))
                | Mult ->
                    if v1Type = "int" then
                        (initIdentifier "int")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " * " ^ v2Type))
                | Div ->
                    if v1Type = "int" then
                        (initIdentifier "int")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " / " ^ v2Type))
                | Mod -> (print_string ("Doing a mod binop\n"));
                    if v1Type = "int" then
                        (initIdentifier "int")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " % " ^ v2Type))
                | And ->
                    if v1Type = "bool" then
                        (initIdentifier "bool")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " and " ^ v2Type))
                | Or ->
                    if v1Type = "bool" then
                        (initIdentifier "bool")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " or " ^ v2Type))
                | Eq ->
                    if v1Type = "int" then
                        (initIdentifier "bool")
                    else raise (Failure ("incorrect type: " ^ v1Type ^ " is " ^ v2Type))
                | NEq ->
                    if v1Type = "int" then
                        (initIdentifier "bool")
                    else
                        raise (Failure ("incorrect type: " ^ v1Type ^ " isnt " ^ v2Type))
                | Less ->
                    if v1Type = "int" then
                        (initIdentifier "bool")
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " < " ^ v2Type))
                | LEq ->
                    if v1Type = "int" then
                        (initIdentifier "bool")
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " <= " ^ v2Type))
                | Greater ->
                    if v1Type = "int" then
                        (initIdentifier "bool")
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " > " ^ v2Type))
                | GEq ->
                    if v1Type = "int" then
                        (initIdentifier "bool")
                    else raise (Failure ("cannot compare: " ^ v1Type ^ " >= " ^ v2Type))
                | _ -> raise (Failure ("Unknown binary operation"))
            ), env, (v1AsJava ^ (string_of_op o) ^ v2AsJava)
        else raise (Failure ("type mismatch: " ^ v1Type ^ " and " ^ v2Type))
    | MethodCall("print", [e]) ->
        let arg, env, eAsJava = eval env e in
            (Bool true), env, ("System.out.println(" ^ eAsJava ^ ")")
    | MethodCall("major", [e; dur]) ->
        let arg, env, eAsJava = eval env e in
        let arg2, env, durAsJava = eval env dur in
            if getType arg = "scale" && getType arg2 = "int" then
                (initIdentifier "chord"), env, ("major(" ^ eAsJava ^ "," ^ durAsJava ^ ")")
            else raise (Failure ("argument of major must be a scale"))
    | MethodCall("minor", [e; dur]) ->
        let arg, env, eAsJava = eval env e in
        let arg2, env, durAsJava = eval env dur in
            if (getType arg = "scale") && (getType arg2 = "int") then
                (initIdentifier "chord"), env, ("minor(" ^ eAsJava ^ "," ^ durAsJava ^ ")")
            else raise (Failure ("argument of minor must be a scale"))
    | MethodCall("sharp", [e]) ->
        let arg, env, eAsJava = eval env e in
            if getType arg = "note" then
                (initIdentifier "note"), env, ("sharp(" ^ eAsJava ^ ")")
            else raise (Failure ("argument of flat must be a note"))
    | MethodCall("flat", [e]) ->
        let arg, env, eAsJava = eval env e in
            if getType arg = "note" then
                (initIdentifier "note"), env, ("flat(" ^ eAsJava ^ ")")
            else raise (Failure ("argument of flat must be a note"))
    | MethodCall("randint", [e]) ->
        let v, env, eAsJava = eval env e in
            if getType v = "int" then
                (initIdentifier "int"), env, ("randint(" ^ eAsJava ^ ")")
            else raise (Failure ("argument of randint must be an integer"))
    | MethodCall("chordOfNote",[e]) ->
        let v, env, eAsJava = eval env e in
            if getType v = "note" then
                (initIdentifier "chord"), env, ("chordOfNote(" ^ eAsJava ^ ")")
            else raise (Failure ("argument of chordOfNote must be a note"))
    | MethodCall("rest", [e]) ->
        let v, env, eAsJava = eval env e in
            if getType v = "int" then
                (initIdentifier "chord"), env, ("rest(" ^ eAsJava ^ ")")
            else raise (Failure ("argument of rest must be an integer"))
    | MethodCall("prepend", [item; alist]) ->
        let arg1, env, itemAsJava = eval env item in
            let arg2, env, listAsJava = eval env alist in
                if getType arg1 = "note" then
                    (if getType arg2 = "scale" then
                        (initIdentifier "scale"), env, ("prepend(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else if getType arg2 = "chord" then (* Returns a new chord with the note appended *)
                        (initIdentifier "chord"), env, ("prepend(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else raise (Failure ("A note can only be prepended to a chord or scale")))
                else if getType arg1 = "chord" then
                    (if getType arg2 = "stanza" then
                        (initIdentifier "stanza"), env, ("prepend(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else raise (Failure ("A chord can only be prepended to a stanza")))
                else if getType arg1 = "stanza" then
                    (if getType arg2 = "score" then
                        (initIdentifier "score"), env, ("prepend(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else raise (Failure ("a stanza can only be prepended to a score")))
                else raise (Failure ("First argument for prepend must be of type note, chord, or stanza"))
    | MethodCall("append", [item; alist]) ->
        let arg1, env, itemAsJava = eval env item in
            let arg2, env, listAsJava = eval env alist in
                if getType arg1 = "note" then
                    (if getType arg2 = "scale" then
                        (initIdentifier "scale"), env, ("append(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else if getType arg2 = "chord" then (* Returns a new chord with the note appended *)
                        (initIdentifier "chord"), env, ("append(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else raise (Failure ("A note can only be appended to a chord or scale")))
                else if getType arg1 = "chord" then
                    (if getType arg2 = "stanza" then
                        (initIdentifier "stanza"), env, ("append(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else raise (Failure ("A chord can only be appended to a stanza")))
                else if getType arg1 = "stanza" then
                    (if getType arg2 = "score" then
                        (initIdentifier "score"), env, ("append(" ^ itemAsJava ^ "," ^ listAsJava ^ ")")
                    else raise (Failure ("a stanza can only be appended to a score")))
                else raise (Failure ("First argument for append must be of type note, chord, or stanza"))
    | MethodCall("concat", [list1; list2]) ->
        let arg1, env, arg1AsJava = eval env list1 in
            let arg2, env, arg2AsJava = eval env list2 in
                if getType arg1 = getType arg2 then
                    (if getType arg1 = "stanza" then
                        (initIdentifier "stanza"), env, ("concat(" ^ arg1AsJava ^ "," ^ arg2AsJava ^ ")")
                    else if getType arg1 = "scale" then
                        (initIdentifier "scale"), env, ("concat(" ^ arg1AsJava ^ "," ^ arg2AsJava ^ ")")
                    else if getType arg1 = "score" then
                        (initIdentifier "score"), env, ("concat(" ^ arg1AsJava ^ "," ^ arg2AsJava ^ ")")
                    else raise (Failure ("concat works only on stanzas, scales, and scores")))
                else raise (Failure ("Both arguments to concat must be of the same type"))
    | MethodCall("repeat", [e; n]) -> (*Takes the argument and returns its container type with arg repeated n times*)
        let arg1, env, eAsJava = eval env e in
            let arg2, env, nAsJava = eval env n in
                if getType arg2 = "int" then
                    (if getType arg1 = "note" then
                        (initIdentifier "scale"), env, ("repeat(" ^ eAsJava ^ "," ^ nAsJava ^ ")")
                    else if getType arg1 = "chord" then
                        (initIdentifier "stanza"), env, ("repeat(" ^ eAsJava ^ "," ^ nAsJava ^ ")")
                    else if getType arg1 = "stanza" then
                        (initIdentifier "score"), env, ("repeat(" ^ eAsJava ^ "," ^ nAsJava ^ ")")
                    else if getType arg1 = "score" then
                        (initIdentifier "score"), env, ("repeat(" ^ eAsJava ^ "," ^ nAsJava ^ ")")
                    else raise (Failure ("The first argument must be a note, chord, stanza, or score")))
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
                                let v, env, _ = ((eval env) actual) in (v :: al), env
                            ) ([], env) e;
        in
        ignore (
            List.map (fun act ->  (* ids need to be scores *)
                        match (getType act) with
                            "score" -> act; (* print_string ("score"^ "\n\n"); *)
                            | _ -> raise (Failure ("compose takes a score only"));
                        ) (List.rev actuals);
        );
        let scoreListAsJava = String.concat "\n" (List.map (fun scor ->
                                            "\n\tadd("^ scor ^");"
                                        ) (List.rev score_names);)
        in Bool true, env, ("\n  compose(new ArrayList<score>() {{" ^ scoreListAsJava ^ "\n}})")
    | MethodCall(name, el) -> (* Check that method exists and passing correct args, do not actually call *)
        (print_string ("User defined method call: " ^ name ^ "\n"));
        let locals, globals, fdecls = env in
            let fdecl = try (NameMap.find name fdecls)
                        with Not_found -> raise (Failure ("Undefined function: " ^ name))
            in
                let actuals, env = List.fold_left
                    (fun (al, env) actual ->
                        let v, env, _ = ((eval env) actual) in (v :: al), env
                    ) ([], env) el
                in
                let actualsAsJava = String.concat "," (List.map(fun arg -> let _, _, asJava = eval env arg in asJava)el) in
                    let l1 =
                        try List.fold_left2 (fun locals formal actual ->
                                                if (getType actual) = (string_of_cbtype formal.paramtype) then
                                                    (NameMap.add formal.paramname (initIdentifier (getType actual)) locals)
                                                else
                                                    raise (Failure ("Wrong parameter type in method call to " ^ fdecl.fname))
                                            ) NameMap.empty fdecl.formals (List.rev actuals)
                        with Invalid_argument(_) -> raise (Failure ("wrong number of arguments to: " ^ fdecl.fname))
                    in (initIdentifier (string_of_cbtype fdecl.rettype)), (l1, globals, fdecls), (name ^ "(" ^ actualsAsJava ^ ")")
    | UnaryOp(uo,e) -> (print_string ("Evaluating a unary op\n"));
        let v, env, eAsJava = eval env e in
        let vType = getType v in
        if ( vType = "note" or vType = "chord" ) then
            (match uo with (* Only accept notes for now *)
                Raise ->
                    if vType = "note" then
                        (initIdentifier "note"), env, (eAsJava ^ ".inc_oct()")
                    else if vType = "chord" then
                            (initIdentifier "chord"), env, (eAsJava ^ ".inc_oct()")
                    else
                        raise (Failure ("cannot raise: " ^ vType))
                | Lower ->
                    if vType = "note" then
                        (initIdentifier "note"), env, (eAsJava ^ ".dec_oct()")
                    else if vType = "chord" then
                        (initIdentifier "chord"), env, (eAsJava ^ ".dec_oct()")
                    else
                        raise (Failure ("cannot raise: " ^ vType)))
        else raise (Failure ("type mismatch: " ^ vType ^ " is not suitable, must be a note or chord"))
    | ListExpr(el) -> (* el is elment list *)
        (print_string ("Evaluating a listExpr\n"));
        let master, _, _ = (eval env (List.hd el)) in (* pull of the first element in el and evalute *)
            let master_type = (getType master) in (* the type of the first element, everything gets compared to this *)
            begin
                match master_type with (* what is the master type? *)
                    "note" -> (* if it is a note create a scale *)
                        let javaStrList = List.map (fun (note_elem) ->
                            (let chord_elem, env, asJava = eval env note_elem in
                                let vType = (getType chord_elem) in
                                    if ( vType = "note") then ("add(" ^ asJava ^ ");")
                                    else raise (Failure ("List expressions must contain all of same type"))
                            )) el in
                        let notesAsJava = String.concat "\n" (List.rev javaStrList) in
                                (initIdentifier "scale"), env, ("new scale(new ArrayList<note>() {{\n" ^ notesAsJava ^ "}})")
                    | "chord" -> (* if it is a chord create a stanza *)
                        let javaStrList = List.map (fun (note_elem) ->
                            (let chord_elem, env, asJava = eval env note_elem in
                                let vType = (getType chord_elem) in
                                    if ( vType = "chord") then ("add(" ^ asJava ^ ");")
                                    else raise (Failure ("List expressions must contain all of same type"))
                            )) el in
                        let chordsAsJava = String.concat "\n" (List.rev javaStrList) in
                                (initIdentifier "stanza"), env, ("new stanza(new ArrayList<chord>() {{\n" ^ chordsAsJava ^ "}})")
                    | "stanza" -> (* if it is a stanza create a score *)
                        let javaStrList = List.map (fun (note_elem) ->
                            (let chord_elem, env, asJava = eval env note_elem in
                                let vType = (getType chord_elem) in
                                    if ( vType = "stanza") then ("add(" ^ asJava ^ ");")
                                    else raise (Failure ("List expressions must contain all of same type"))
                            )) el in
                        let stanzasAsJava = String.concat "\n" (List.rev javaStrList) in
                                (initIdentifier "score"), env, ("new score(new ArrayList<stanza>() {{\n" ^ stanzasAsJava ^ "}})")
                    | _ -> raise (Failure ("List expression must only contain notes or chords or stanzas"))
            end
    | Assign(toE, fromE) ->
        let lft_expr, env, lft_expr_jString = eval env toE in
            let rht_expr, (locals, globals, fdecls), rht_expr_jString = eval env fromE in
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
                                                rht_expr, (NameMap.add (fst lftName) rht_expr locals, globals, fdecls), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                            else if snd lftType = "globals" then
                                                rht_expr, (locals, NameMap.add (fst lftName) rht_expr globals, fdecls), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                            else raise (Failure ("fatal error")))
                                        (* MEMBER METHODS *)
                                        else if lftIdType = "member" then
                                            (* NOTE MEMBER METHODS *)
                                            if fst lftType = "note" then
                                                if snd lftName = "pitch" then
                                                    if getInt rht_expr >= 0 && getInt rht_expr <= 127 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).pitch <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).pitch <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                        else raise (Failure ("fatal error"))
                                                    else raise (Failure ("invalid note pitch: " ^ string_of_int (getInt rht_expr) ^ ". pitch must be between 0-127."))
                                                else if snd lftName = "duration" then (* min max checking *)
                                                    if snd lftType = "locals" then
                                                        rht_expr, (((getNote (NameMap.find (fst lftName) locals)).duration <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                    else if snd lftType = "globals" then
                                                        rht_expr, (((getNote (NameMap.find (fst lftName) globals)).duration <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                    else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                else if snd lftName = "octave" then (* min max checking *)
                                                    if getInt rht_expr >= -5 && getInt rht_expr <= 5 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).octave <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).octave <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                        else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                    else raise (Failure ("invalid note octave: " ^ string_of_int (getInt rht_expr) ^ ". octave must be between -5-5."))
                                                else raise (Failure ("fatal error"))
                                            else if fst lftType = "chord" then
                                                if snd lftName = "chord_duration" then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) locals)).duration <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getNote (NameMap.find (fst lftName) globals)).duration <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                        else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                else raise (Failure ("fatal error"))
                                            else if fst lftType = "score" then
                                                if snd lftName = "instrument" then
                                                    if getInt rht_expr >= 0 && getInt rht_expr <= 127 then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getScore (NameMap.find (fst lftName) locals)).instrument <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                    else if snd lftType = "globals" then
                                                        rht_expr, (((getScore (NameMap.find (fst lftName) globals)).instrument <- getInt rht_expr); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                                    else raise (Failure ("fatal error"))
                                                else raise (Failure ("invalid score instrument: " ^ string_of_int (getInt rht_expr) ^ ". instrument must be between 0-127."))
                                            else raise (Failure ("fatal error"))
                                            else raise (Failure ("cannot assign to: " ^ fst lftType))
                                        else raise (Failure ("cannot assign to: " ^ (fst lftType)))
                                    | "scale" ->
                                        if lftIdType = "id" then
                                            (if snd lftType = "locals" then
                                                rht_expr, (NameMap.add (fst lftName) rht_expr locals, globals, fdecls), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                            else if snd lftType = "globals" then
                                                rht_expr, (locals, NameMap.add (fst lftName) rht_expr globals, fdecls), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                            else raise (Failure ("fatal error")))
                                        (* MEMBER METHODS *)
                                        else if lftIdType = "member" then
                                            (* NOTE MEMBER METHODS *)
                                            if fst lftType = "scale" then
                                                let str_len = (length rht_expr_jString - 10) in

                                                if snd lftName = "scale_notelist" then
                                                        if snd lftType = "locals" then
                                                            rht_expr, (((getChord (NameMap.find (fst lftName) locals)).notelist <- (getScale rht_expr).scale_notelist); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ (String.sub rht_expr_jString 9 str_len) )
                                                        else if snd lftType = "globals" then
                                                            rht_expr, (((getChord (NameMap.find (fst lftName) globals)).notelist <- (getScale rht_expr).scale_notelist); (locals, globals, fdecls)), ("\n\t" ^ lft_expr_jString ^ " = " ^ (String.sub rht_expr_jString 9 str_len) )
                                                        else raise (Failure ("undeclared identifier: " ^ fst lftName))
                                                else raise (Failure ("fatal error"))
                                            else raise (Failure ("cannot assign to: " ^ fst lftType))
                                        else raise (Failure ("cannot assign to: " ^ (fst lftType)))
                                    | _ -> (* bool, note, chord, staff, part *)
                                        if lftIdType = "id" then
                                            (if snd lftType = "locals" then
                                                rht_expr, (NameMap.add (fst lftName) rht_expr locals, globals, fdecls), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                            else if snd lftType = "globals" then
                                                rht_expr, (locals, NameMap.add (fst lftName) rht_expr globals, fdecls), ("\n\t" ^ lft_expr_jString ^ " = " ^ rht_expr_jString)
                                            else raise (Failure ("fatal error")))
                                        else raise (Failure ("cannot assign to: " ^ (fst lftType)))
                            else if lftIdType = "id" then
                                raise (Failure ("cannot assign: " ^ fst lftType ^ " = " ^ rhtType))
                            else if lftIdType = "member" then
                                raise (Failure ("cannot assign: " ^ lftRetType ^ " = " ^ rhtType))
                            else raise (Failure ("fatal error"))
    | NoExpr -> Bool true, env, ""
and exec env fname = function
        Expr(e) -> let _, env, asJava = (eval env e) in
            (print_string ("Exec an expr\n"));
            env, (asJava ^ ";\n")
        | Return(e) -> (print_string ("Return stmt in exec\n"));
            let v, (locals, globals, fdecls), asJava = (eval env e) in
                let fdecl = NameMap.find fname fdecls in
                    if (getType v) = (string_of_cbtype fdecl.rettype) then
                        (locals, globals, fdecls), ("return " ^ asJava ^ ";\n")
                    else raise (Failure ("function " ^ fdecl.fname ^ " returns: " ^ (getType v) ^ " instead of " ^ (string_of_cbtype fdecl.rettype)))
        | Block(s1) -> (print_string ("Block stmt in exec\n"));
            let (locals, globals, fdecls) = env in
                let (l, g), jStr = call s1 locals globals fdecls fname "" (* Check to make sure we don't need a rev *)
                in (l, g, fdecls), jStr
        | If(e, ibl, s) -> (print_string ("If stmt with else in exec\n"));
            let (locals, globals, fdecls) = env in
                let v, env, evalJavaString = eval env e in
                    if (getType v) = "bool" then (env, ("if(" ^ evalJavaString ^ ") {\n" ^ (snd (call (List.rev ibl) locals globals fdecls fname "")) ^ "}\n else {\n" ^ (snd ((exec env fname) s)) ^ "}\n"))
                    else raise (Failure ("If statement must be given boolean expression"))
        | If(e, s, Block([])) -> (print_string ("If stmt without else in exec\n"));
            let (locals, globals, fdecls) = env in
                let v, env, evalJavaString = eval env e in
                    if (getType v) = "bool" then (env, ("if(" ^ evalJavaString ^ ") {\n" ^ (snd (call (List.rev s) locals globals fdecls fname "")) ^ "}\n"))
                    else raise (Failure ("If statement must be given boolean expression"))
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
                                let (locals, globals), jStr = (call (sl) (NameMap.add par_decl.paramname (initIdentifier "note") locals) globals fdecls fname "") in                                    
                                    (env, "for (note " ^ par_decl.paramname ^ " : " ^ list_name ^ ".notelist " ^ ") { " ^ jStr ^ " } ")
                            else
                                raise (Failure ("failure of type matching with chord list"))
                        | "scale" ->
                            (*notes*)
                            if (string_of_cbtype par_decl.paramtype) = "note" then
                                let (locals, globals), jStr = (call (sl) (NameMap.add par_decl.paramname (initIdentifier "note") locals) globals fdecls fname "") in                                    
                                    (env, "for (note " ^ par_decl.paramname ^ " : " ^ list_name ^ ".scale_notelist " ^ ") { " ^ jStr ^ " } ")
                            else
                                raise (Failure ("failure of type matching with scale list"))

                       | "stanza" ->
                            (*chords*)
                            if (string_of_cbtype par_decl.paramtype) = "chord" then
                                let (locals, globals), jStr = (call (sl) (NameMap.add par_decl.paramname (initIdentifier "chord") locals) globals fdecls fname "") in                                    
                                    (env, "for (chord " ^ par_decl.paramname ^ " : " ^ list_name ^ ".chordlist " ^ ") { " ^ jStr ^ " } ")
                            else
                                raise (Failure ("failure of type matching with stanza list"))
                        | "score" ->
                             if (string_of_cbtype par_decl.paramtype) = "stanza" then
                                let (locals, globals), jStr = (call (sl) (NameMap.add par_decl.paramname (initIdentifier "stanza") locals) globals fdecls fname "") in                                    
                                    (env, "for (stanza " ^ par_decl.paramname ^ " : " ^ list_name ^ ".stanzalist " ^ ") { " ^ jStr ^ " } ")
                            else
                                raise (Failure ("failure of type matching with score list"))
                        | _ ->
                            raise (Failure ("undesired list type for for_each loop"))
                end
        | While(e, sl) ->
            let boolarg, env, javaString = eval env e in
                if (getType boolarg) = "bool" then (
                    let locals, globals, fdecls = env in
                        let (locals, globals), jStr = call (List.rev sl) locals globals fdecls fname "" in
                            (locals, globals, fdecls), ("while(" ^ javaString ^ ") {\n" ^ jStr ^ "}\n")
                )
                else raise (Failure ("while loop argument must decompose to a boolean value"))
        | _ -> raise (Failure ("Unable to match the statment"))
(* Execute the body of a method and return an updated global map *)
and call fdecl_body locals globals fdecls fdecl_name jStr=
        match fdecl_body with
            [] -> (print_string ("Done with a full call\n"));
                (locals, globals), jStr (*When we are done return the updated globals*)
            | head::tail ->
                match head with
                    VDecl2(head) -> (print_string ("Working on a vdecl in call\n"));
                        if(fdecl_name = "") then
                            ((if NameMap.mem head.varname globals then raise (Failure ("Variable " ^ head.varname ^ " declared twice")));
                                (call tail locals (NameMap.add head.varname (initIdentifier (string_of_cbtype head.vartype)) globals) fdecls fdecl_name (jStr ^ ("\n" ^ (string_of_cbtype head.vartype) ^ " " ^ head.varname ^ ";\n"))))
                        else
                            ((if NameMap.mem head.varname locals then raise (Failure ("Variable " ^ head.varname ^ " declared twice")));
                                print_string ("\n\n<" ^ head.varname ^ ">");
                                call tail (NameMap.add head.varname (initIdentifier (string_of_cbtype head.vartype)) locals) globals fdecls fdecl_name
                                (jStr ^ ("\n" ^ (string_of_cbtype head.vartype) ^ " "  ^ head.varname ^ ";\n")))
                    | FullDecl2(head) -> (print_string ("Working on a full decl in call\n"));
                        (if(fdecl_name = "") then (if NameMap.mem head.fvname globals then raise (Failure ("Variable " ^ head.fvname ^ " declared twice"))));
                        (if(fdecl_name <> "") then (if NameMap.mem head.fvname locals then raise (Failure ("Variable " ^ head.fvname ^ " declared twice"))));
                        let v, env, rhsJavaString = eval (locals, globals, fdecls) head.fvexpr in
                            let vType = getType v in
                                if vType = (string_of_cbtype head.fvtype)
                                    then
                                        match vType with
                                            "int" -> print_string ("\n\n<" ^ head.fvname ^ ">"); call tail (NameMap.add head.fvname (Int (getInt v)) locals) globals fdecls fdecl_name (jStr ^ ("int " ^ head.fvname ^ " = " ^ rhsJavaString ^ ";\n"))
                                            | "note" -> call tail (NameMap.add head.fvname (Note (getNote v)) locals) globals fdecls fdecl_name (jStr ^ ("note " ^ head.fvname ^ " = " ^ rhsJavaString ^ ";\n"))
                                            | "chord" -> call tail (NameMap.add head.fvname (Chord (getChord v)) locals) globals fdecls fdecl_name (jStr ^ ("chord " ^ head.fvname ^ " = " ^ rhsJavaString ^ ";\n"))
                                            | "bool" -> call tail (NameMap.add head.fvname (Bool (getBool v)) locals) globals fdecls fdecl_name (jStr ^ ("bool " ^ head.fvname ^ " = " ^ rhsJavaString ^ ";\n"))
                                            | "scale" -> call tail (NameMap.add head.fvname (Scale (getScale v)) locals) globals fdecls fdecl_name (jStr ^ ("scale " ^ head.fvname ^ " = " ^ rhsJavaString ^ ";\n"))
                                            | "stanza" -> call tail (NameMap.add head.fvname (Stanza (getStanza v)) locals) globals fdecls fdecl_name (jStr ^ ("stanza " ^ head.fvname ^ " = " ^ rhsJavaString ^ ";\n"))
                                            | "score" -> call tail (NameMap.add head.fvname (Score (getScore v)) locals) globals fdecls fdecl_name (jStr ^ ("score " ^ head.fvname ^ " = " ^ rhsJavaString ^ ";\n"))
                                            | _ -> raise (Failure ("Unknown type: " ^ vType))
                                else
                                    raise (Failure ("LHS = " ^ (string_of_cbtype head.fvtype) ^ " <> RHS = " ^ vType))
                    | Stmt2(head) -> (print_string ("Working on a stmt in call\n"));
                        let (locals, globals, fdecls), execJavaString = (exec (locals, globals, fdecls) fdecl_name head) in
                            call tail locals globals fdecls fdecl_name execJavaString
and translate prog env =
    let locals, globals, fdecls = env in
        match prog with
            [] -> (* everything went well, write the java file and quit *)
                (print_string ("Finished the program\n"));
                (print_string globalJava.contents);
                (print_string methJava.contents);
                (print_string run_start);
                (print_string mainJava.contents);
                let javaOut = open_out ("Cb.java") in
                Printf.fprintf javaOut "%s" (import_decl ^ class_start ^ globalJava.contents ^ methJava.contents ^ main_start ^ run_start ^ mainJava.contents ^ run_end ^ class_end);
                (close_out javaOut);
                Bool true, (locals, globals, fdecls)
            | head::tail ->
                match head with
                    VDecl(head) -> (print_string ("Translate sees a vdecl\n"));
                        (if NameMap.mem head.varname globals then raise (Failure ("Variable " ^ head.varname ^ " defined more than once"))
                        else globalJava := globalJava.contents ^ "\n" ^ (string_of_cbtype head.vartype) ^ " " ^ head.varname ^ ";\n");
                        translate tail (locals, (NameMap.add head.varname (initIdentifier (string_of_cbtype head.vartype)) globals), fdecls)
                    | FullDecl(head) -> (print_string ("Translate sees a fulldecl\n"));
                        (if NameMap.mem head.fvname globals then raise (Failure ("Variable " ^ head.fvname ^ " defined more than once")));
                        let v, env, asJava = eval (locals, globals, fdecls) head.fvexpr in
                            let vType = getType v in
                                if vType = (string_of_cbtype head.fvtype)
                                    then
                                        ((globalJava := globalJava.contents ^ "\n" ^ (string_of_cbtype head.fvtype) ^ " " ^ head.fvname ^ " = " ^ asJava ^ ";\n");
                                        match vType with
                                            "int" -> translate tail (locals, (NameMap.add head.fvname (Int (getInt v)) globals), fdecls)
                                            | "note" -> translate tail (locals, (NameMap.add head.fvname (Note (getNote v)) globals), fdecls)
                                            | "chord" -> translate tail (locals, (NameMap.add head.fvname (Chord (getChord v)) globals), fdecls)
                                            | "bool" -> translate tail (locals, (NameMap.add head.fvname (Bool (getBool v)) globals), fdecls)
                                            | "scale" -> translate tail (locals, (NameMap.add head.fvname (Scale (getScale v)) globals), fdecls)
                                            | "stanza" -> translate tail (locals, (NameMap.add head.fvname (Stanza (getStanza v)) globals), fdecls)
                                            | "score" -> translate tail (locals, (NameMap.add head.fvname (Score (getScore v)) globals), fdecls)
                                            | _ -> raise (Failure ("Unknown type: " ^ vType)))
                                else
                                    (raise (Failure ("LHS = " ^ (string_of_cbtype head.fvtype) ^ "<> RHS = " ^ vType)))
                    | MDecl(head) -> (print_string ("Translate sees a methdecl\n"));
                        (if (NameMap.mem head.fname fdecls) then raise (Failure ("Method with same name already defined"))
                        else
                            let newlocals = List.fold_left(fun acc arg -> (NameMap.add arg.paramname (initIdentifier (string_of_cbtype arg.paramtype)) acc)) locals head.formals in
                            let (_, _), javaBody = call head.body newlocals globals (NameMap.add head.fname head fdecls) head.fname "" in
                                (methJava := methJava.contents ^ "\npublic " ^ (string_of_cbtype head.rettype) ^ " " ^ head.fname ^ "(" ^
                                (String.concat "," (List.map(fun arg -> (string_of_cbtype arg.paramtype) ^ " " ^ arg.paramname)(List.rev head.formals))) ^
                                ") {" ^ javaBody ^ "\n}\n");
                            translate tail (locals, globals, (NameMap.add head.fname head fdecls))
                        )
                    | Stmt(head) -> (print_string ("Translate sees a stmt\n"));
                        let env, jString = exec (locals, globals, fdecls) "" head in
                            (mainJava := mainJava.contents ^ jString);
                            translate tail env

let helper prog = translate prog (NameMap.empty, NameMap.empty, NameMap.empty)
