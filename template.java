
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
}

class scale {

    public ArrayList<note> scale_notelist;

    public scale() {
        scale_notelist = new ArrayList<note>();
    }

    public scale(ArrayList<note> snl) {
        scale_notelist = snl;
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
}

class Cb {

    /*
     * All global variables should be put here once declared
     * but we still have to check that when you access them they exist
     * because java won't do that for us
     */
    /**
     * ********************compose helper functions****************************
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

//		for(int i=0;i<data.size();i++)
//			System.out.println(data.get(i));


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
                velocity = 90; // this is the volume of the sound

        for (int channel = 0; channel < data.size(); channel++) {
            //populating the ith track
            ArrayList<stanza> stan = data.get(channel).stanzalist;
            for (int tr = 0; tr < stan.size(); tr++) {

                ArrayList<chord> chl = stan.get(tr).chordlist;
                for (int cl = 0; cl < chl.size(); cl++) { //chord list

                    ArrayList<note> tnote = chl.get(cl).notelist;
                    for (int nti = 0; nti < tnote.size(); nti++) { //note list

                        duration = (int) (chl.get(cl).chord_duration * (5 / 16));  //second number is duration
                        // octave (-5 to 5); pitch (0 to 11)
                        nt = (int) map((long) (tnote.get(nti).pitch + tnote.get(nti).octave), -5, 16, 0, 127); //this is the pitch representation; middle c = c_4 = 60
                        velocity = 127;  //velocity can not be changed for now

                        if (tnote.get(nti).pitch < 0) { // a rest is received -- any negative note is rest

                            nt = 0;
                            track[channel].add(createNoteOffEvent(nt, tick, channel));				//add note to this track

                        } else {
                            track[channel].add(createNoteOnEvent(nt, tick, channel, velocity));				//add note to this track
                        }
                        tick = tick + duration;  //first number is tick
                        track[channel].add(createNoteOffEvent(nt, tick + duration, channel));
                    }
                    tick = tick + duration;
                }
            }
            tick = 0;
        }

        
        // Print track information
//		System.out.println();
//		if ( track != null ) {
//			for ( int i = 0; i < track.length; i++ ) {
//				System.out.println( "Track " + i + ":" );
//		
//     		for ( int j = 0; j < track[i].size(); j++ ) {
//					MidiEvent event = track[i].get( j );
// 					System.out.println(" tick "+event.getTick()+", "+MessageInfo.toString(event.getMessage()));
// 				} 
// 			} 
//		} 

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
        System.out.println("(" + n.pitch + "," + n.octave + "," + n.duration + ")");
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
        return 0;
    }

    public static chord chordOfNote(note n) {
        return new chord();
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
    /*
     * End of built in methods
     */

    /*
     * Since Cb doesn't have a main method, most of the code in your Cb program
     * will end up in javas main method unless it is a global variable declaration
     * or a method declaration
     */
    public static void main(String[] args) {
        //This is what we fill in as we translate the program
        //all of your statements go here
        //Method declarations should be put outside of this following the
        //example below
        //Note we have to be very wary of globally scoped things since this
        //is java, if you don't understand I will explain on tuesday night
    }
}


/*
 * Warning, need to be careful with globals
 *
 * Example method declaration into java
 * example.cb ->
 *
 * meth void doIt(note n, scale s)
 *     foreach(note r in s)
 *         print(r);
 *     end
 * end
 *
 * in java ->
 *
 * public static void doIt(Note n, Scale s) {
 *     foreach(Note r : s) {
 *         print(r);
 *     }
 * }
 */