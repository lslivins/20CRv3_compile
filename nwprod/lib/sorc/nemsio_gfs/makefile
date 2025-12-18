LIB     = libnemsiogfs.a
INCMOD  = ./incmod
FC      = xlf95_r
FFLAGS  = -O3  -I /nwprod/lib/incmod/nemsio
AR      = ar
ARFLAGS = -rv -X64

MODULES  = nemsio_gfs.o

$(LIB): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)
	mv *.mod $(INCMOD)

clean:
	rm -f *.o *.mod $(LIB)

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

