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
  print "Program Was:\n" + output
  print "\n=====Calling OCaml Cb=====\n"
  ocaml = subprocess.Popen(['./Cb'],stdin=subprocess.PIPE,stdout=subprocess.PIPE)
  ocaml.stdin.write(output+"\n")
  ocaml.stdin.close()
  while True:
    line = ocaml.stdout.readline()
    if line != '':
      print line
    else:
      break
  print "\n=====PYTHON COMPILER COMPLETE=====\n"

if len(sys.argv) != 2:
    print "Error: please provide the compiler with a file to compile"
    sys.exit()

if os.path.isfile(sys.argv[1]):
  compile(sys.argv[1])
else:
  print "Error: " + sys.argv[1] + " not found"