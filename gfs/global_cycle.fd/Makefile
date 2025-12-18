FCS=ftn
FCP=ftn
OPTFLAGS=-O3 -convert big_endian
FREEFLAG=-FR
DOUBLEFLAG=-r8
OMPFLAG=-qopenmp-stubs
SHELL=	/bin/sh
LIBS    =-L../../nwprod/lib -lsfcio_4 -L../../nwprod/lib -lsp_d -lw3lib-2.0_d  -lbacio_4
INCS=-I../../nwprod/lib/incmod/sfcio_4 
FFLAGS  = $(INCS) $(OPTFLAGS) $(DOUBLEFLAG)  $(OMPFLAG) -traceback
CMD = global_cycle
global_cycle:	num_parthds.o machine.o cycle.o sfcsub.o
	${FCS} $(FFLAGS) $(OMPFLAG) -o $(CMD) num_parthds.o cycle.o sfcsub.o machine.o $(LIBS)
num_parthds.o:	num_parthds.f
	$(FCS) $(FFLAGS) $(OMPFLAG) -c num_parthds.f
machine.o:	machine.f
	${FCS} $(FFLAGS) -c machine.f
sfcsub.o:	sfcsub.f
	${FCS} $(FFLAGS) -c sfcsub.f
cycle.o:	cycle.f
	${FCS} $(FFLAGS) -c cycle.f
clean:
	-rm -f *.o *.mod  $(CMD)
