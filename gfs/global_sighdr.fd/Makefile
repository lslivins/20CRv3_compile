SHELL	=/bin/sh
INCMOD	=../../nwprod/lib/incmod/sigio_4
SRCM	=sighdr.f
INCM	=
SRCS	=
OBJS	=
MODS	=
FC	=ftn
FFLAGS	=-O3  -FR -qopenmp -convert big_endian
LDFLAGS	=   
LIBS    =  -L../../nwprod/lib -lsigio_4 -lw3lib-2.0_4
CMD	=global_sighdr
$(CMD):	$(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS) $(MODS) $(CMD)
clobber:        clean
	-rm -f $(CMD)
