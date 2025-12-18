SHELL=	/bin/ksh
INCMOD  =$(SFCIO_INC4)
FC      =$(FCMP)
OBJS	=num_parthds.o machine.o cycle.o sfcsub.o
LIBS    = -L$(SFCIO_DIR) -l$(SFCIO_LIB4) -L$(W3EMC_DIR) -l$(W3EMC_LIBd) -L$(W3NCO_DIR) -l$(W3NCO_LIBd) -L$(BACIO_DIR) -l$(BACIO_LIB4) -L$(SP_DIR) -l$(SP_LIBd)
CMD     =global_cycle
$(CMD):	$(OBJS)
	${FC} ${DEBUG} $(FFLAGS) $(LDFLG) $(OBJS) $(LIBS) -o $(CMD)

num_parthds.o:	num_parthds.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c num_parthds.f

machine.o:	machine.f
	${FC} $(FFLAGS) -c machine.f

sfcsub.o:	sfcsub.f
	$(FC)  -I $(INCMOD) $(FFLAGS) $(OMPFLAG) -c sfcsub.f

cycle.o:	cycle.f
	$(FC)  -I $(INCMOD) $(FFLAGS) -c cycle.f

install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) *.mod
