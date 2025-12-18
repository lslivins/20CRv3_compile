SHELL	=/bin/sh
SRCM	=sfchdr.f
INCM	=
SRCS	=
OBJS	=
MODS	=
FC	=xlf90
FFLAGS	=-qnosave -O2 -qarch=604 -qmaxmem=-1
LDFLAGS	=-bmaxdata:384000000 -bmaxstack:256000000
LIBS	=/nwprod/w3lib90/bacio_4_604 /nwprod/w3lib90/w3lib_4_604
CMD	=global_sfchdr
$(CMD):	$(SRCM) $(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS) $(MODS)
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) $(INCM) makefile
