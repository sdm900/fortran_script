#
# Makefile for fortran_script (gfortran)
#
# Verified with system gfortran (15.x). The old g95 toolchain
# (~/bin/g95 plus -fpointer=invalid -freal=nan -ftrace=full) is gone.
#
# Targets:
#   make         build bin/check and bin/shell
#   make check   build the test runner (bin/check)
#   make shell   build the interactive interpreter (bin/shell)
#   make test    build bin/check and run the suite (from the repo root)
#   make clean   remove objects, binaries and generated dependencies
#
# Overrides, e.g.:
#   make F90FLAGS="-O0 -g -fcheck=all -Wall"
#
# Notes:
#   - src/*.f90 use `#include "macros.h"`; gfortran handles that
#     directly with -cpp -I./src (no external cpp staging step).
#   - check.o and shell.o each define main and must never be linked
#     together: bin/check links check.o (plus checkscrpt.o), bin/shell
#     links shell.o instead of check.o.
#   - src/params.f90 is generated from src/params_m4.f90 via m4;
#     edit the _m4 file, not params.f90.
#

#
# Shell to execute commands in
#
SHELL = /bin/sh

#
# Local variables
#
OBJDIR =	obj
SRCDIR =	src
BINDIR =	bin
CHECK = 	check
FSH =		shell
SRC :=		$(shell cat $(SRCDIR)/LIST)
BINSRC =	$(SRCDIR)/$(CHECK).f90 $(SRCDIR)/$(FSH).f90
OBJ :=		$(SRC:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)

#
#  Compile options
#
F90COMPILER ?=	gfortran
LINKER =	$(F90COMPILER)
F90FLAGS ?=	-O2
INCLUDES =	-I./$(SRCDIR) -I./$(OBJDIR)
F90MOD =	-J./$(OBJDIR)
F90LIBS =
M4 =		m4

.PHONY: all $(CHECK) $(FSH) test clean

#
# Build binaries
#
all : $(BINDIR)/$(CHECK) $(BINDIR)/$(FSH)
$(CHECK) : $(BINDIR)/$(CHECK)
$(FSH) : $(BINDIR)/$(FSH)

$(BINDIR)/$(CHECK) : $(OBJ) $(OBJDIR)/$(CHECK).o | $(BINDIR)
	$(LINKER) $(F90FLAGS) $(OBJ) $(OBJDIR)/$(CHECK).o $(F90LIBS) -o $@

$(BINDIR)/$(FSH) : $(OBJ) $(OBJDIR)/$(FSH).o | $(BINDIR)
	$(LINKER) $(F90FLAGS) $(OBJ) $(OBJDIR)/$(FSH).o $(F90LIBS) -o $@

#
# m4 processed files
#
$(SRCDIR)/params.f90 : $(SRCDIR)/params_m4.f90
	$(M4) $< > $@

#
# Build objects
#
$(OBJDIR)/%.o : $(SRCDIR)/%.f90 | $(OBJDIR)
	$(F90COMPILER) -c $(F90FLAGS) -cpp $(INCLUDES) $(F90MOD) $< -o $@

$(OBJDIR) $(BINDIR) :
	mkdir -p $@

#
# Run the test suite (must run from the repo root: the runner
# hardcodes tests/<name>.in / .out / .exp relative paths)
#
test : $(BINDIR)/$(CHECK)
	timeout -s KILL 60 ./$(BINDIR)/$(CHECK)

#
# Dependencies
#
$(shell ./build_deps)

define DEPTEMP
$(1) : $(2)
endef

$(foreach prog,$(SRC),$(eval $(call DEPTEMP,$(prog),$(shell cat $(prog:$(SRCDIR)/%.f90=$(SRCDIR)/.%.dep)))))

$(foreach prog,$(BINSRC),$(eval $(call DEPTEMP,$(prog),$(shell cat $(prog:$(SRCDIR)/%.f90=$(SRCDIR)/.%.dep)))))

#
# Clean
#
clean :
	rm -rf $(OBJDIR) $(BINDIR) $(SRCDIR)/.*.dep
