
JFLAGS = -g
JC = javac



.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
        CSV2MIDI.java \
        CSV.java \
        listInstruments.java \
        MessageInfo.java



default: classes

classes: $(CLASSES:.java=.class)

package: default
	echo Main-Class: CSV2MIDI > MANIFEST.INF
	jar cvfm CSV2MIDI.jar MANIFEST.INF *.class "midi instruments.csv" sorted_insrument.csv

.PHONY: clean
clean:
	$(RM) ~* *.jar *.class
	
.PHONY: clean
all: clean default