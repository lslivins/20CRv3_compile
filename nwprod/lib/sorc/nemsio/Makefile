LIB     = $(LIBS)/libnemsio.a
INCMOD  = $(LIBS)/incmod/nemsio
FC      = $(FCMP)
FFLAGS  = $(FFLAGM)
FFLAGB  = $(FFLAGBM)
AR      = ar
ARFLAGS = -ruv

MODULES  = bafrio.o nemsio_openclose.o nemsio_read.o nemsio_write.o nemsio_module.o

$(LIB): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)
	mkdir -p $(INCMOD)
	mv *.mod $(INCMOD)
	rm -f *.o 

clean:
	rm -f *.o *.mod $(LIB) $(INCMOD)/*.mod
	
.SUFFIXES:
.SUFFIXES: .f .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

.f.o:
	$(FC) $(FFLAGB) -c $*.f

bafrio.o: bafrio.f
nemio_module.o: nemio_module.f90 nemsio_openclose.o nemsio_read.o nemsio_write.o
nemsio_openclose.o : nemsio_openclose.f90 bafrio.o
nemsio_read.o: nemsio_read.f90 nemsio_openclose.o
nemsio_write.o: nemsio_write.f90 nemsio_openclose.o
