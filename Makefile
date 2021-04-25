# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "utils" library for array concatenation

.PHONY : all
all : seaflow.native utils.o

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
	rm -f seaflowparse.ml
	rm -f seaflowparse.mli
	rm -f *.o
	
# Building the array concatenation util

utils : utils.c
	cc -o utils utils.c

# Building the tarball

TESTS = \
  basic1 declarations

FAILS = \
  decl

TESTFILES = $(TESTS:%=test-%.flo) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.flo) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags seaflow.ml seaflowparse.mly \
	scanner.mll semant.ml testall.sh \
	utils.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=test_seaflow/%) 

seaflow.tar.gz : $(TARFILES)
	cd .. && tar czf proj/seaflow.tar.gz \
		$(TARFILES:%=proj/%)
