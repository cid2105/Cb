import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import javax.sound.midi.InvalidMidiDataException;
import javax.sound.midi.MidiEvent;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Sequence;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.Track;
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
        return "(" + pitch + "," + octave + "," + duration + ")";
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
        String s = "([";
        for(note n : notelist) {
            s += n.toString();
        }
        return s + "], " + chord_duration + ")";
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
        String s = "[";
        for(note n : scale_notelist) {
            s += n.toString();
        }
        return s + "]";
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
        String s = "[";
        for(chord c : chordlist) {
            s += c.toString();
        }
        return s + "]";
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
        String s = "([";
        for(stanza st : stanzalist) {
            s += st.toString();
        }
        return s + "], instrument=" + instrument + ")";
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
    long map(long x, long in_min, long in_max, long out_min, long out_max) {
        return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
    }
    /*
     * note representation: tick, duration, pitch, volume/loudness/velocity
     * turns note on
     */

    private MidiEvent createNoteOnEvent(int nKey, long lTick, int channel, int velocity) {
        return createNoteEvent(ShortMessage.NOTE_ON, nKey, velocity, lTick, channel);
    }

    /*
     * turns note off
     */
    private MidiEvent createNoteOffEvent(int nKey, long lTick, int channel) {
        return createNoteEvent(ShortMessage.NOTE_OFF, nKey, 0, lTick, channel);  //set note to 0 velocity
    }

    /*
     * turns note on or off
     */
    private MidiEvent createNoteEvent(int nCommand, int nKey, int nVelocity, long lTick, int channel) {
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
    public void compose(ArrayList<score> data) throws InvalidMidiDataException {
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
            System.out.println("Instrument set to " + instrument[inst] + " on channel " + inst);
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

                        duration = (int) ((chl.get(cl).chord_duration / 4) );  //second number is duration
                        System.out.println("dur:-" + duration + "> \n " );
                        // octave (-5 to 5); pitch (0 to 11)
                        nt = (4 + tnote.get(nti).octave) * 12 + tnote.get(nti).pitch;//(int) map((long) (tnote.get(nti).pitch + tnote.get(nti).octave), -5, 16, 0, 127); //this is the pitch representation; middle c = c_4 = 60
                        velocity = 120;  //velocity can not be changed for now

                        if (tnote.get(nti).pitch < 0) { // a rest is received -- any negative note is rest
                            nt = 0;
                            //track[channel].add(createNoteOffEvent(nt, tick, channel));              //add note to this track

                        } else {
                            track[channel].add(createNoteOnEvent(nt, tick, channel, velocity));             //add note to this track
                        }
                        // tick = tick + duration;  //first number is tick
                        // track[channel].add(createNoteOffEvent(nt, tick + duration, channel));
                    }
                    tick = tick + duration;
                     System.out.println("tick:-" + tick + "> \n " + duration);
                }
            }
            tick = 0;
        }


        // Print track information
        // Print track information

        System.out.println();

        if (track != null) {

            for (int i = 0; i < track.length; i++) {

                System.out.println("Track " + i + ":");



                for (int j = 0; j < track[i].size(); j++) {

                    MidiEvent event = track[i].get(j);

                    //System.out.println(" tick " + event.getTick() + ", " + MessageInfo.toString(event.getMessage()));

                }

            }

        }


        /* Now we just save the Sequence to the file we specified.
         The '0' (second parameter) means saving as SMF type 0.
         (type 1 is for multiple tracks).
         */
        try {
            MidiSystem.write(sequence, 1, new File("out.mid"));
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    // assumes it is passed a minor scale
    public chord major(scale s) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception("To convert a scale into a major chord, you must pass a scale of at least five notes");

        int duration = s.scale_notelist.get(0).duration;

        return new chord(notes, duration);
    }

    public chord major(scale s, int duration) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception("To convert a scale into a major chord, you must pass a scale of at least five notes");
        return new chord(notes, duration);
    }

    public chord minor(scale s) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception("To convert a scale into a minor chord, you must pass a scale of at least five notes");
        int duration = s.scale_notelist.get(0).duration;
        return new chord(notes, duration);
    }

    public chord minor(scale s, int duration) throws Exception {
        ArrayList<note> notes = new ArrayList<note>();
        if(s.scale_notelist.size() >= 5){
            notes.add(s.scale_notelist.get(0));
            notes.add(flat(s.scale_notelist.get(2)));
            notes.add(s.scale_notelist.get(4));
        }
        else
            throw new Exception("To convert a scale into a minor chord, you must pass a scale of at least five notes");
        return new chord(notes, duration);
    }


    public note sharp(note n) throws Exception {
        note sharped = new note();
        if(n.pitch == 11)
            sharped = new note(0, n.octave+1, n.duration);
        else
            sharped = new note(n.pitch + 1, n.octave, n.duration);
        if(sharped.isValid())
            return sharped;
        else
            throw new Exception("To convert a scale into a minor chord, you must pass a scale of at least five notes");
    }

    public note flat(note n) {
        return new note();
    }

    /**
     * Return a random integer between 0 and i
     * @param i The maximum value for the integer
     * @return A random integer between 0 and i
     */
    public int randint(int i) {
        return (int)(Math.random()*i);
    }

    public chord chordOfNote(note n) {
        return n.toChord();
    }

    public chord rest(int d) {
        ArrayList<note> temp = new ArrayList<note>(1);
        temp.add(new note(-1, 0, d));
        return new chord(temp, d);
    }

    public chord prepend(note n, chord c) {
        chord tmp = c.deepCopy();
        tmp.notelist.add(0, n.deepCopy());
        return tmp;
    }

    public scale prepend(note n, scale s) {
        scale tmp = s.deepCopy();
        tmp.scale_notelist.add(0, n.deepCopy());
        return tmp;
    }

    public stanza prepend(chord c, stanza s) {
        stanza tmp = s.deepCopy();
        tmp.chordlist.add(0, c.deepCopy());
        return tmp;
    }

    public score prepend(stanza st, score sc) {
        score tmp = sc.deepCopy();
        tmp.stanzalist.add(0, st.deepCopy());
        return tmp;
    }

    public chord append(note n, chord c) {
        chord tmp = c.deepCopy();
        tmp.notelist.add(n.deepCopy());
        return tmp;
    }

    public scale append(note n, scale s) {
        scale tmp = s.deepCopy();
        tmp.scale_notelist.add(n.deepCopy());
        return tmp;
    }

    public stanza append(chord c, stanza s) {
        stanza tmp = s.deepCopy();
        tmp.chordlist.add(c.deepCopy());
        return tmp;
    }

    public score append(stanza st, score sc) {
        score tmp = sc.deepCopy();
        tmp.stanzalist.add(st.deepCopy());
        return tmp;
    }

    public scale concat(scale s1, scale s2) {
        scale tmp = s1.deepCopy();
        for(note n : s2.scale_notelist) {
            tmp.scale_notelist.add(n.deepCopy());
        }
        return tmp;
    }

    public stanza concat(stanza s1, stanza s2) {
        stanza tmp = s1.deepCopy();
        for(chord c : s2.chordlist) {
            tmp.chordlist.add(c.deepCopy());
        }
        return tmp;
    }

    public score concat(score s1, score s2) {
        score tmp = s1.deepCopy();
        for(stanza s : s2.stanzalist) {
            tmp.stanzalist.add(s.deepCopy());
        }
        return tmp;
    }

    public scale repeat(note n, int i) throws Exception {
        scale tmp = new scale();
        if (i < 1) {
            throw new Exception("repeat function takes an integer that must be 1 or greater");
        }
        for(int j = 0; j < i; j++) {
            tmp.scale_notelist.add(n.deepCopy());
        }
        return tmp;
    }

    public stanza repeat(chord c, int i) throws Exception {
        stanza tmp = new stanza();
        if (i < 1) {
            throw new Exception("repeat function takes an integer that must be 1 or greater");
        }
        for(int j = 0; j < i; j++) {
            tmp.chordlist.add(c.deepCopy());
        }
        return tmp;
    }

    public score repeat(stanza s, int i) throws Exception{
        score tmp = new score();
        if (i < 1) {
            throw new Exception("repeat function takes an integer that must be 1 or greater");
        }
        tmp.instrument = 0;
        for(int j = 0; j < i; j++) {
            tmp.stanzalist.add(s.deepCopy());
        }
        return tmp;
    }

    public score repeat(score s, int i) throws Exception{
        score tmp = new score();
        if (i < 1) {
            throw new Exception("repeat function takes an integer that must be 1 or greater");
        }
        for(int j = 0; j < i; j++) {
            for(stanza st : s.stanzalist) {
                tmp.stanzalist.add(st.deepCopy());
            }
        }
        tmp.instrument = s.instrument;
        return tmp;
    }


int eighth;

note g_8;

note g_16;

note high_g;

note low_g;

note d;

note d_8;

note low_d;

note high_D;

note e;

note high_E;

note low_e;

note low_e_8;

note e_8;

note c;

note c_8;

note b;

note high_B;

note low_b;

note a;

note high_a;

note f;

note low_c;

note bass_low_g;

note bass_mid_d;

note bass_mid_g;

note bass_mid_b;

note b5;

note m1;

note mel_mid_b;

note m3;

note bass_low_d;

note bass_low_a;

note bass_mid_f_sharp;

note bass_low_d_quarter;

note mel_high_b;

note mel_high_c;

note mel_high_b_dotted_eighth;

note bass_low_c;

note bass_mid_c;

note mel_high_a;

note mel_high_a_dotted_eighth;

chord db;

chord da;

chord da_48;

chord dg;

chord gd_48;

chord gd_q;

chord gd_e;

chord double_e;

chord low_double_e;

chord eb;

chord norm_eb;

chord fb;

chord double_b;

chord up_double_b;

chord double_a;

chord double_g;

chord low_double_g;

chord double_c;

chord double_d;

chord cg;

score piano_bass;

score piano_treble;

stanza s1;

stanza bass1;

stanza bass2;

stanza bass3;

stanza bass4;

stanza bass5;

stanza bass6;

stanza bass7;

stanza bass8;

stanza bass9;

stanza bass10;

stanza bass11;

stanza verse1;

stanza verse2;

stanza verse3;

stanza triplet;

chord accent;

chord dotted_a;

stanza q_rest;

stanza verse4;

stanza verse5;

stanza verse6;

int dotted_eight;

int sixteenth;

stanza bm_1;

chord chord_m1;

chord chord_m2;

chord chord_m3;

stanza mm_1;

stanza bm_2;

stanza mm_2;

stanza bm_3;

stanza mm_3;

stanza bm_4;

stanza mm_4;

stanza terminal;

public stanza startRest() {stanza tmp = new stanza(new ArrayList<chord>() {{
add(rest(eighth));}});
return tmp;

}

public stanza raise_scale_pitch_by_one(scale s) {
stanza temp_stanza = new stanza();
for (note n : s.scale_notelist ) { 
	n.pitch  = n.pitch +1;

	temp_stanza = append(chordOfNote(n),temp_stanza);
 } return temp_stanza;

}

public stanza make_stanza_from_scale(scale s) {
stanza temp_stanza = new stanza();
for (note n : s.scale_notelist ) { 
	temp_stanza = append(chordOfNote(n),temp_stanza);
 } return temp_stanza;

}

    public static void main(String[] args) throws Exception { Cb runner = new Cb(); runner.run(); }
public void run() throws Exception {
eighth = 16/2;

g_8 =  new note(7,0,16/2);

g_16 =  new note(7,0,16);

high_g =  new note(7,1,16);

low_g =  new note(7,-1,16);

d =  new note(2,0,16);

d_8 =  new note(2,0,eighth);

low_d =  new note(2,-1,16);

high_D =  new note(2,1,16);

e =  new note(4,0,16);

high_E =  new note(4,1,16);

low_e =  new note(4,-1,16);

low_e_8 =  new note(4,-1,16);

e_8 =  new note(4,0,eighth);

c =  new note(0,-1,16);

c_8 =  new note(0,-1,eighth);

b =  new note(11,0,16);

high_B =  new note(11,1,16);

low_b =  new note(11,-1,16);

a =  new note(9,0,16);

high_a =  new note(9,1,16);

f =  new note(5,0,16);

low_c =  new note(0,-1,16);

bass_low_g =  new note(7,-1,eighth);

bass_mid_d =  new note(2,0,eighth);

bass_mid_g =  new note(7,0,eighth);

bass_mid_b =  new note(11,1,16);

b5 =  new note(7,-1,16);

m1 =  new note(7,2,eighth);

mel_mid_b =  new note(11,1,eighth);

m3 =  new note(2,2,eighth);

bass_low_d =  new note(2,-1,eighth);

bass_low_a =  new note(9,0,eighth);

bass_mid_f_sharp =  new note(6,1,16);

bass_low_d_quarter =  new note(2,-1,16);

mel_high_b =  new note(11,2,eighth);

mel_high_c =  new note(0,3,eighth);

mel_high_b_dotted_eighth =  new note(11,2,12);

bass_low_c =  new note(0,-1,eighth);

bass_mid_c =  new note(0,0,eighth);

mel_high_a =  new note(9,2,eighth);

mel_high_a_dotted_eighth =  new note(9,2,eighth);

db = new chord(new ArrayList<note>() {{
add(b);
add(d);}}, eighth);

da = new chord(new ArrayList<note>() {{
add(a);
add(d);}}, eighth);

da_48 = new chord(new ArrayList<note>() {{
add(a);
add(d);}}, 48);

dg = new chord(new ArrayList<note>() {{
add(g_16);
add(d);}}, eighth);

gd_48 = new chord(new ArrayList<note>() {{
add(g_8);
add(high_D);}}, 48);

gd_q = new chord(new ArrayList<note>() {{
add(g_8);
add(high_D);}}, 16);

gd_e = new chord(new ArrayList<note>() {{
add(g_8);
add(high_D);}}, eighth);

double_e = new chord(new ArrayList<note>() {{
add(e);
add(high_E);}}, 16);

low_double_e = new chord(new ArrayList<note>() {{
add(e);
add(low_e);}}, 16);

eb = new chord(new ArrayList<note>() {{
add(high_B);
add(high_E);}}, 48);

norm_eb = new chord(new ArrayList<note>() {{
add(high_B);
add(e);}}, 48);

fb = new chord(new ArrayList<note>() {{
add(f);
add(b);}}, 48);

double_b = new chord(new ArrayList<note>() {{
add(b);
add(low_b);}}, 16);

up_double_b = new chord(new ArrayList<note>() {{
add(b);
add(high_B);}}, eighth);

double_a = new chord(new ArrayList<note>() {{
add(a);
add(high_a);}}, eighth);

double_g = new chord(new ArrayList<note>() {{
add(high_g);
add(g_8);}}, eighth);

low_double_g = new chord(new ArrayList<note>() {{
add(low_g);
add(g_8);}}, 16);

double_c = new chord(new ArrayList<note>() {{
add(c);
add(low_c);}}, 16);

double_d = new chord(new ArrayList<note>() {{
add(low_d);
add(d);}}, 16);

cg = new chord(new ArrayList<note>() {{
add(high_g);
add(c);}}, 48);

piano_bass = new score();

piano_treble = new score();

s1 = repeat(chordOfNote(g_8),8);

bass1 = repeat(chordOfNote(g_16),8);

bass2 = repeat(chordOfNote(d),4);

bass3 = repeat(chordOfNote(e),4);

bass4 = repeat(chordOfNote(c),4);

bass5 = repeat(chordOfNote(g_8),8);

bass6 = repeat(chordOfNote(d_8),8);

bass7 = repeat(chordOfNote(e_8),8);

bass8 = repeat(chordOfNote(c_8),4);

	bass8 = concat(bass8,repeat(chordOfNote(c),2));

bass9 = new stanza(new ArrayList<chord>() {{
add(low_double_g);
add(gd_48);}});

bass10 = new stanza(new ArrayList<chord>() {{
add(double_d);
add(da_48);}});

bass11 = new stanza(new ArrayList<chord>() {{
add(low_double_e);
add(norm_eb);}});

verse1 = startRest();

	verse1 = concat(verse1,repeat(db,5));

	verse1 = append(da,verse1);

	verse1 = append(db,verse1);

verse2 = startRest();

	verse2 = concat(verse2,repeat(db,5));

	verse2 = concat(verse2,repeat(da,2));

verse3 = startRest();

	verse3 = concat(verse3,repeat(dg,3));

triplet = new stanza(new ArrayList<chord>() {{
add(new chord(new ArrayList<note>() {{
add(g_16);
add(d);}}, 11));
add(new chord(new ArrayList<note>() {{
add(d);
add(high_D);}}, 10));
add(new chord(new ArrayList<note>() {{
add(b);
add(d);}}, 11));
add(rest(8));}});

	verse3 = concat(verse3,triplet);

accent = chordOfNote(b);

	accent.chord_duration  = 4;

dotted_a = chordOfNote(a);

	dotted_a.chord_duration  = 12;

q_rest = new stanza(new ArrayList<chord>() {{
add(rest(16));}});

verse4 = new stanza(new ArrayList<chord>() {{
add(gd_q);
add(gd_e);
add(accent);
add(dotted_a);
add(chordOfNote(g_8));}});

verse5 = new stanza(new ArrayList<chord>() {{
add(gd_q);}});

verse6 = repeat(double_g,6);

	verse6 = append(double_a,verse6);

	verse6 = append(up_double_b,verse6);

dotted_eight = 12;

sixteenth = 16/4;

bm_1 = make_stanza_from_scale(new scale(new ArrayList<note>() {{
add(bass_low_g);
add(bass_mid_d);
add(bass_mid_g);
add(bass_mid_d);
add(bass_mid_b);
add(b5);}}));

chord_m1 = new chord(new ArrayList<note>() {{
add(mel_mid_b);
add(m1);}}, 16);

chord_m2 = new chord(new ArrayList<note>() {{
add(mel_mid_b);}}, eighth);

chord_m3 = new chord(new ArrayList<note>() {{
add(m3);}}, sixteenth);

mm_1 = new stanza(new ArrayList<chord>() {{
add(rest(16));
add(chord_m1);
add(chord_m2);
add(chord_m3);
add(new chord(new ArrayList<note>() {{
add(mel_mid_b);
add(m1);}}, 12));
add(chordOfNote(m3));}});

bm_2 = make_stanza_from_scale(new scale(new ArrayList<note>() {{
add(bass_low_d);
add(bass_low_a);
add(bass_mid_d);
add(bass_low_a);
add(bass_mid_f_sharp);
add(bass_low_d_quarter);}}));

	bm_2 = prepend(rest(eighth*3),bm_2);

mm_2 = make_stanza_from_scale(new scale(new ArrayList<note>() {{
add( new note(-1,0,24));
add(mel_mid_b);
add(mel_mid_b);
add( new note(2,2,sixteenth));
add(mel_high_b_dotted_eighth);
add(m1);}}));

bm_3 = make_stanza_from_scale(new scale(new ArrayList<note>() {{
add(low_e_8);
add(b);
add(e);
add(b);
add(g_16);
add(low_e);}}));

mm_3 = make_stanza_from_scale(new scale(new ArrayList<note>() {{
add( new note(-1,0,24));
add( new note(4,2,eighth));
add(mel_high_b);
add(mel_high_c);
add(mel_high_b);
add(m1);}}));

bm_4 = make_stanza_from_scale(new scale(new ArrayList<note>() {{
add(bass_low_c);
add(bass_low_g);
add(bass_mid_c);
add(bass_low_g);
add( new note(4,1,16));
add( new note(0,-1,16));
add( new note(-1,0,24));}}));

mm_4 = make_stanza_from_scale(new scale(new ArrayList<note>() {{
add( new note(-1,0,24));
add( new note(7,2,eighth));
add(mel_high_b);
add(mel_high_a);
add(mel_high_a_dotted_eighth);
add( new note(7,2,eighth));
add( new note(-1,0,24));}}));

terminal = new stanza(new ArrayList<chord>() {{
add(chordOfNote( new note(4,1,eighth)));}});

	piano_bass = new score(new ArrayList<stanza>() {{
add(bass1);
add(bass2);
add(bass3);
add(bass4);
add(bass5);
add(bass6);
add(bass7);
add(bass8);
add(bass9);
add(bass10);
add(bass11);
add(bm_1);
add(bm_2);
add(bm_3);
add(bm_4);
add(terminal);}});

	piano_treble = new score(new ArrayList<stanza>() {{
add(s1);
add(verse1);
add(verse1);
add(verse2);
add(verse3);
add(verse1);
add(verse1);
add(verse2);
add(verse3);
add(q_rest);
add(verse4);
add(verse5);
add(verse4);
add(verse5);
add(verse4);
add(verse6);
add(mm_1);
add(mm_2);
add(mm_3);
add(mm_4);
add(terminal);}});

  compose(new ArrayList<score>() {{
	add(piano_bass);

	add(piano_treble);
}});

    }

}
