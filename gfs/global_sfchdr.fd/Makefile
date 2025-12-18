SHELL	=/bin/sh
INCMOD	=../../nwprod/lib/incmod/sfcio_4
SRCM	=sfchdr.f
INCM	=
SRCS	=
OBJS	=
MODS	=
FC	=ftn
FFLAGS	=-O3 -xSSE4.2 -FR -qopenmp -convert big_endian
LDFLAGS	=   
LIBS    = -L../../nwprod/lib -lsfcio_4  -lw3lib-2.0_4
CMD	=global_sfchdr
$(CMD):	$(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS) $(MODS) $(CMD)
