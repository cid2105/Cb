#! /usr/bin/python

import sys
import os
import subprocess

def compile(fName):
  proc = subprocess.Popen(['python','preprocessor.py', fName],stdout=subprocess.PIPE)
  output = ""
  while True:
    line = proc.stdout.readline()
    if line != '':
      output += line
    else:
      break
  print "Calling OCaml Cb"
  ocaml = subprocess.Popen(['Cb'],stdin=PIPE,stdout=subprocess.PIPE)
  ocaml.stdin.write(output+"\n")
  while True:
    line = ocaml.stdout.readline()
    if line != '':
      print line
    else:
      break
  print "PYTHON COMPILER COMPLETE"

if len(sys.argv) != 2:
    print "Error: please provide the compiler with a file to compile"
    sys.exit()

if os.path.isfile(sys.argv[1]):
  compile(sys.argv[1])
else:
  print "Error: " + sys.argv[1] + " not found"