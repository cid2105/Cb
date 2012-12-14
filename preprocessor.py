#!/usr/bin/python

import sys
import os
import re

comments = re.compile(r'<-.*?->')
uses = re.compile(r'use (.*?);')

def proc(fName):
    try:
        with open(fName) as f:
            asArray = f.readlines()
            progString = "\n".join(asArray)
            without_comments = comments.sub('', progString)
            for match in uses.search(without_comments):
                if os.path.isfile(match.group(1)):
                    try:
                        with open() as f2:
                            toArr = f2.readlines()
                            toStr = "\n".join(toArr)
                            progString = re.replace(match.group(0), toStr)
                    except IOError:
                        print "Preprocessor Error: use statement file: " + match.group(1) + " not found"
                else:
                    print "Preprocessor Error: use statement file: " + match.group(1) + " not found"
                    sys.exit()
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
