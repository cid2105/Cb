#! /usr/bin/python
#Cb language compiler, by Matthew Cowan

import sys
import os
import subprocess

verbose = False

# def findOutput(searchFor, fileType):
#   outputFiles = []
#   counter = 0
#   while os.path.isfile(searchFor + str(counter) + fileType):
#     if verbose: print "Found file: " + searchFor + str(counter) + fileType
#     outputFiles.append(searchFor + str(counter) + fileType)
#     counter += 1
#   return outputFiles

def compile(fName):
  if verbose: print "=====Running The PreProcessor=====\n"
  proc = subprocess.Popen(['python','preprocessor.py', fName],stdout=subprocess.PIPE)
  output = ""
  while True:
    line = proc.stdout.readline()
    if line != '':
      output += line.rstrip() + "\n"
    else:
      break
  if verbose: print "Program Was:\n" + output
  if verbose: print "=====Calling OCaml Cb=====\n"
  ocaml = subprocess.Popen(['./Cb'],stdin=subprocess.PIPE,stdout=subprocess.PIPE)
  ocaml.stdin.write(output)
  ocaml.stdin.close()
  while True:
    line = ocaml.stdout.readline()
    if line != '':
      print line.rstrip()
    else:
      break
  if verbose: print "\n=====Ocaml COMPILER COMPLETE=====\n"
  # if verbose: print "\n=====Finding & Compiling Output\n"
  # toJava = findOutput("musiccb", ".csv")
  # for f in toJava:
  #   java = subprocess.Popen(['java', '-jar', 'CSV2MIDI.jar', f, f[:-4] + ".mid"],stdout=subprocess.PIPE)
  #   while True:
  #     line = java.stdout.readline()
  #     if line != '':
  #       if verbose: print line.rstrip()
  #     else:
  #       break
  #   if verbose: print "\n"
  # if verbose: print "\n=====MIDI Files Created=====\n"
  if verbose: print "\n=====Compilation Complete=====\n"

def ast(fName):
  print "=====Running The PreProcessor=====\n"
  proc = subprocess.Popen(['python','preprocessor.py', fName],stdout=subprocess.PIPE)
  output = ""
  while True:
    line = proc.stdout.readline()
    if line != '':
      output += line.rstrip() + "\n"
    else:
      break
  print "Program Was:\n" + output
  print "=====Calling OCaml Cb=====\n"
  ocaml = subprocess.Popen(['./Cb', '-a'],stdin=subprocess.PIPE,stdout=subprocess.PIPE)
  ocaml.stdin.write(output)
  ocaml.stdin.close()
  while True:
    line = ocaml.stdout.readline()
    if line != '':
      print line.strip()
    else:
      break
  print "\n=====Ocaml COMPILER COMPLETE=====\n"
  print "\n=====Compilation Complete=====\n"

if len(sys.argv) == 1:
  print "Error: please provide the compiler with a file to compile"
  sys.exit()
elif len(sys.argv) == 2:
  if os.path.isfile(sys.argv[1]):
    compile(sys.argv[1])
  else:
    print "Error: " + sys.argv[1] + " not found"
elif len(sys.argv) == 3:
  if sys.argv[1].lower() == "-a":
    ast(sys.argv[2])
  elif sys.argv[1].lower() == "-i":
    compile(sys.argv[2])
  elif sys.argv[1].lower() == "-v":
    verbose = True
    compile(sys.argv[2])
  elif sys.argv[2].lower() == "-a":
    ast(sys.argv[1])
  elif sys.argv[2].lower() == "-i":
    compile(sys.argv[1])
  elif sys.argv[2].lower() == "-v":
    verbose = True
    compile(sys.argv[1])
  else:
    print "Error: usage python compile.py [compiler flag] <cb file>"
    sys.exit()
else:
  print "Error: usage python compile.py [compiler flag] <cb file>"
  sys.exit()