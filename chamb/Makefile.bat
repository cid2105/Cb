ocamllex scanner.mll    
ocamlyacc parser.mly  
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c calc.ml
ocamlc -o mycalc.exe graphics.cma scanner.cmo parser.cmo calc.cmo

