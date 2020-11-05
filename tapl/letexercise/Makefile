# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable file f
#   make windows to rebuild the executable file f.exe
#   make test    to rebuild the executable and run it on input file test.f
#   make clean   to remove all intermediate and temporary files
#   make depend  to rebuild the intermodule dependency graph that is used
#                  by make to determine which order to schedule 
#	           compilations.  You should not need to do this unless
#                  you add new modules or new dependencies between 
#                  existing modules.  (The graph is stored in the file
#                  .depend)

# Flags for the OCaml compiler
#
OCAMLVERSION = $(shell ocamlc -vnum)
OCAMLVERSION_MAJOR = $(shell echo $(OCAMLVERSION) | cut -f1 -d.)
OCAMLVERSION_MINOR = $(shell echo $(OCAMLVERSION) | cut -f2 -d.)
OCAMLVERSION_GE_4_02 := $(shell [ $(OCAMLVERSION_MAJOR) -gt 4 -o \( \
	$(OCAMLVERSION_MAJOR) -eq 4 -a $(OCAMLVERSION_MINOR) -ge 2 \) ] && echo true)

ifeq ($(OCAMLVERSION_GE_4_02), true)
	OCAMLCFLAGS = -unsafe-string
else
	OCAMLCFLAGS =
endif

# These are the object files needed to rebuild the main executable file
#
OBJS = support.cmo syntax.cmo core.cmo parser.cmo lexer.cmo main.cmo

# Files that need to be generated from other files
DEPEND += lexer.ml parser.ml 

# When "make" is invoked with no arguments, we build an executable 
# typechecker, after building everything that it depends on
all: $(DEPEND) $(OBJS) f

# On a Windows machine, we do exactly the same except that the executable
# file that gets built needs to have the extension ".exe"
windows: $(DEPEND) $(OBJS) f.exe

# Include an automatically generated list of dependencies between source files
include .depend

# Build an executable typechecker
f: $(OBJS) main.cmo 
	@echo Linking $@
	ocamlc -o $@ $(COMMONOBJS) $(OBJS) 

# Build an executable typechecker for Windows
f.exe: $(OBJS) main.cmo 
	@echo Linking $@
	ocamlc -o $@ $(COMMONOBJS) $(OBJS) 

# Build and test
test: all
	./f test.f

# Compile an ML module interface
%.cmi : %.mli
	ocamlc -c $(OCAMLCFLAGS) $<

# Compile an ML module implementation
%.cmo : %.ml
	ocamlc -c $(OCAMLCFLAGS) $<

# Generate ML files from a parser definition file
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	ocamlyacc -v parser.mly
	@chmod -w parser.ml parser.mli

# Generate ML files from a lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Clean up the directory
clean::
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output \
	   f f.exe TAGS *~ *.bak

# Rebuild intermodule dependencies
depend:: $(DEPEND) 
	ocamldep $(INCLUDE) *.mli *.ml > .depend

# 
