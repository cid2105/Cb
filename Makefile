OBJS = scanner.cmo parser.cmo ast.cmo interpret.cmo Cb.cmo

cb : $(OBJS)
	ocamlc -o Cb.exe $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

cb.tar.gz : $(TARFILES)
	cd .. && tar czf cb/cb.tar.gz $(TARFILES:%=cb/%)

.PHONY : clean
clean :
	rm -f *~ cb parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff

.PHONY : all
all : clean cb

# Generated by ocamldep *.ml *.mli
ast.cmo:
ast.cmx:
interpret.cmo: ast.cmo
interpret.cmx: ast.cmx
Cb.cmo: scanner.cmo parser.cmi interpret.cmo \
    ast.cmo
Cb.cmx: scanner.cmx parser.cmx interpret.cmx \
    ast.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo