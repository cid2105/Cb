#!/usr/bin/python
#Cb language preprocessor, by Matthew Cowan

import sys
import os
import re

comments = re.compile(r'<-.*?->')
uses = re.compile(r'use (.*?);')

def proc(fName):
    try:
        with open(fName) as f:
            asArray = f.readlines()
            progString = "".join(asArray)
            without_comments = comments.sub('', progString)
            for match in uses.findall(without_comments):
                if os.path.isfile(match):
                    try:
                        with open(match) as f2:
                            toArr = f2.readlines()
                            toStr = "".join(toArr)
                            progString = re.sub("use " + match + ";",toStr,progString)
                    except IOError:
                        print "Preprocessor Error: use statement file: " + match + " not found"
                else:
                    print "Preprocessor Error: use statement file: " + match + " not found"
                    sys.exit()
            print progString
    except IOError:
        print "Preprocessor Error: file provided not found"
        sys.exit()


if len(sys.argv) != 2:
    print "Preprocessor Error: needs to take name of file"
    sys.exit()

if os.path.isfile(sys.argv[1]):
    proc(sys.argv[1])
else:
    print "Preprocessor Error: file provided not found"
