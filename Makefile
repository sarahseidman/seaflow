# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : seaflow.native printbig.o

# "make seaflow.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

seaflow.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind seaflow.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
	rm -f *.cmo
	rm -f *.cmi

.PHONY : intermediate
intermediate :
	ocamllex scanner.mll
	ocamlyacc seaflowparse.mly
	ocamlc -c ast.ml
	ocamlc -c sast.ml
	ocamlc -c seaflowparse.mli
	ocamlc -c scanner.ml
	ocamlc -c seaflowparse.ml
	ocamlc -c semant.ml
	ocamlc -c seaflow.ml
	ocamlc -o seaflow seaflowparse.cmo scanner.cmo seaflow.cmo sast.cmo semant.cmo

	
# Testing the "printbig" example

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

# Building the tarball

TESTS = \
  basic1 declarations

FAILS = \
  decl

TESTFILES = $(TESTS:%=test-%.flo) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.flo) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags seaflow.ml seaflowparse.mly \
	scanner.mll semant.ml testall.sh \
	printbig.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=test_seaflow/%) 

seaflow.tar.gz : $(TARFILES)
	cd .. && tar czf proj/seaflow.tar.gz \
		$(TARFILES:%=proj/%)
