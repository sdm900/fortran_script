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
CPPDIR = 	cpp
CHECK = 	check
FSH =		shell
SRC :=		$(shell cat $(SRCDIR)/LIST)
BINSRC =	$(SRCDIR)/$(CHECK).f90 $(SRCDIR)/$(FSH).f90
OBJ :=		$(SRC:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)

#
# Search path for command binaries
#
export PATH:=$(PATH):/sw/bin:/opt/g95-install/bin

#
#  Compile options
#

#
# g95
#
F90COMPILER =   ~/bin/g95
LINKER =	~/bin/g95
F90FLAGS =      -g -fbounds-check -fpointer=invalid -freal=nan -ftrace=full # -O4 -falign-loops -ffast-math -floop-optimize2 -funroll-loops -maltivec -mpowerpc -finline-functions #
LDDFLAGS = 	#-framework Foundation -framework AppKit
INCLUDES =      -I./$(SRCDIR) #-I/usr/local/include 
F90INC =        -I./$(OBJDIR) $(INCLUDES) -fmod=./$(OBJDIR)
F90LIBS =       #-L/sw/lib -L/sw/lib/pgplot -L/usr/X11R6/lib -lpgplot -lpng -lg2c -laquaterm -lX11
F90DEFINES = 

#
# Extra parsing stuff
#
CPP =           cpp -P -C -traditional-cpp
M4 =            m4



#
# Build binary
#
$(CHECK) : $(BINDIR)/$(CHECK)
$(FSH) : $(BINDIR)/$(FSH)

$(BINDIR)/$(CHECK) : $(OBJ) $(OBJDIR)/$(CHECK).o
	$(LINKER) $(F90FLAGS) $(F90INC) $(OBJ) $(OBJDIR)/$(CHECK).o $(LDDFLAGS) -o $@ $(F90LIBS)

$(BINDIR)/$(FSH) : $(OBJ) $(OBJDIR)/$(FSH).o
	$(LINKER) $(F90FLAGS) $(F90INC) $(OBJ) $(OBJDIR)/$(FSH).o $(LDDFLAGS) -o $@ $(F90LIBS)

#
# m4 processed files
#
$(SRCDIR)/params.f90: $(SRCDIR)/params_m4.f90
	$(M4) $< > $@

#
# Build objects
#
$(OBJDIR)/%.o : $(SRCDIR)/%.f90
	$(CPP) $(INCLUDES) < $< | awk '/^[^#]/' > $(CPPDIR)/$(<F) 
	$(F90COMPILER) -c $(F90FLAGS) $(F90INC) $(CPPDIR)/$(<F) -o $@

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
clean:
	rm -rf obj/* bin/* cpp/* src/.*.dep



