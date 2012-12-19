
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

    public String toString()  {
        return "(" + pitch + "," + octave + "," + duration + ")";
    }

    public chord toChord() {
        ArrayList<note> tmp_list = new ArrayList<note>(1);
        tmp_list.add(new note(pitch, octave, duration));
        return new chord(tmp_list, duration);
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
}

class score {

    public ArrayList<stanza> stanzalist;
    public int instrument;

    public score() {
        stanzalist = new ArrayList<stanza>();
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
}

public class Cb {
    
    static long map(long x, long in_min, long in_max, long out_min, long out_max) {
        return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
    }

    private static MidiEvent createNoteOnEvent(int nKey, long lTick, int channel, int velocity) {
        return createNoteEvent(ShortMessage.NOTE_ON, nKey, velocity, lTick, channel);
    }

    private static MidiEvent createNoteOffEvent(int nKey, long lTick, int channel) {
        return createNoteEvent(ShortMessage.NOTE_OFF, nKey, 0, lTick, channel); 
    }

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
    
    public static void compose(ArrayList<score> data) throws InvalidMidiDataException {
        int nChannels = data.size();
        Sequence sequence = null;

        int timingRes = 4, instrument[] = new int[nChannels];

        for (int inst = 0; inst < data.size(); inst++) {
            instrument[inst] = data.get(inst).instrument;
            System.out.println("Instrument set to " + instrument[inst] + " on channel " + inst);
        }

        try {
            sequence = new Sequence(Sequence.PPQ, timingRes);
        } catch (InvalidMidiDataException e) {
            e.printStackTrace();
            System.exit(1);
        }

        Track track[] = new Track[nChannels];
        for (int i = 0; i < nChannels; i++) {
            track[i] = sequence.createTrack();                 

            ShortMessage sm = new ShortMessage();
            sm.setMessage(ShortMessage.PROGRAM_CHANGE, i, instrument[i], 0);
        }

        int nt = 0,
                tick = 0,
                duration = 5,
                velocity = 90;
        for (int channel = 0; channel < data.size(); channel++) {
            ArrayList<stanza> stan = data.get(channel).stanzalist;
            for (int tr = 0; tr < stan.size(); tr++) {

                ArrayList<chord> chl = stan.get(tr).chordlist;
                for (int cl = 0; cl < chl.size(); cl++) {

                    ArrayList<note> tnote = chl.get(cl).notelist;
                    for (int nti = 0; nti < tnote.size(); nti++) {

                        duration = (int) (chl.get(cl).chord_duration * (5 / 16));
                        nt = (int) map((long) (tnote.get(nti).pitch + tnote.get(nti).octave), -5, 16, 0, 127); 

                        if (tnote.get(nti).pitch < 0) { 

                            nt = 0;
                            track[channel].add(createNoteOffEvent(nt, tick, channel));              

                        } else {
                            track[channel].add(createNoteOnEvent(nt, tick, channel, velocity));
                        }
                        tick = tick + duration;
                        track[channel].add(createNoteOffEvent(nt, tick + duration, channel));
                    }
                    tick = tick + duration;
                }
            }
            tick = 0;
        }

        try {
            MidiSystem.write(sequence, 1, new File("out.mid"));
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    public static void print(boolean b) {
        if (b) {
            System.out.println("true");
        } else {
            System.out.println("false");
        }
    }

    public static void print(int i) {
        System.out.println(i);
    }

    public static void print(note n) {
        System.out.println(n);
    }

    public static void print(chord c) {
        System.out.println(c);
    }

    public static void print(scale s) {
        System.out.println(s);
    }

    public static void print(stanza s) {
        System.out.println(s);
    }

    public static void print(score s) {
        System.out.println(s);
    }

    public static chord major(scale s) {
        return new chord();
    }

    public static chord minor(scale s) {
        return new chord();
    }

    public static note sharp(note n) {
        return new note();
    }

    public static note flat(note n) {
        return new note();
    }

    public static int randint(int i) {
        return (int)(Math.random()*i);
    }

    public static chord chordOfNote(note n) {
        return new n.toChord();
    }

    public static chord rest(int d) {
        ArrayList<note> temp = new ArrayList<note>(1);
        temp.add(new note(-1, 0, d));
        return new chord(temp, d);
    }

    public static chord prepend(note n, chord c) {
        return new chord();
    }

    public static scale prepend(note n, scale s) {
        return new scale();
    }

    public static stanza prepend(chord c, stanza s) {
        return new stanza();
    }

    public static score prepend(stanza st, score sc) {
        return new score();
    }

    public static chord append(note n, chord c) {
        return new chord();
    }

    public static scale append(note n, scale s) {
        return new scale();
    }

    public static stanza append(chord c, stanza s) {
        return new stanza();
    }

    public static score append(stanza st, score sc) {
        return new score();
    }

    public static scale concat(scale s1, scale s2) {
        return new scale();
    }

    public static stanza concat(stanza s1, stanza s2) {
        return new stanza();
    }

    public static score concat(score s1, score s2) {
        return new score();
    }

    public static scale repeat(note n, int i) {
        return new scale();
    }

    public static stanza repeat(chord c, int i) {
        return new stanza();
    }

    public static score repeat(stanza s, int i) {
        return new score();
    }

    public static score repeat(score s, int i) {
        return new score();
    }

    }

}
