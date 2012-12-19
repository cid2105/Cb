/*	Testing the functionality of m built-in objects. */

def create_note( int a, float b, int c)
	note n = Note(a, b, c);	
	return n;
end

void main()
{
	note n1;
	note n2;
	note n3;
	chord c;
	staff s;
	part p;
	
	//Create a note by setting field by field
	n1.pitch = As7;
	n1.duration = 0.5;
	n1.intensity = 100;

	//Create a note from another note
	n2 = n1;
	
	//Create a note with a function
	n3 = create_note( Cf2, 1.0, 100);

	//Set staff field
	s.instrument = 123;
	
	//Set part fields
	p.bpm = 60;
	p.beatsig = 1.0;
	
	//Use the built-in add function to add notes to chords
	add(c, n2);
	add(c, n3);
	
	//Use the built-in add function to add notes and chords to staffs
	add(s, n1);
	add(s, c);
	
	//Use the built-in add function to add staffs to parts
	add(p, s);
	
	//Use the built-in play function to indicate what part(s) you
	//want played in the output
	play(p);
}
