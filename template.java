import java.util.ArrayList;


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

public class Cb {

    /*
     * All global variables should be put here once declared
     * but we still have to check that when you access them they exist
     * because java won't do that for us
     */


    /*
     * Here are all of the built in methods that we have and while we
     * are translating we need to make sure you do not overwrite any of these
     * bad boys unless you give it different args (or maybe we can just let
     * java take care of that, I am not fully certain)
     */
    public static void compose() {

    }

    public static void print(boolean b) {
        if(b) {
            System.out.println("true");
        }
        else {
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
        temp.add(new note(-1,0,d));
        return new chord(temp,d);
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