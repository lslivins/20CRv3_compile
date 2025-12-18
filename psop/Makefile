FCP           = ftn   
FCS           = ftn   
FFLAGS        = -O3 -assume byterecl -warn all -implicitnone -traceback -assume byterecl -convert big_endian
FREEFLAG      = -FR
COREINC       = ../nwprod/lib/incmod
INCsigio      = $(COREINC)/sigio_4
INCsfcio      = $(COREINC)/sfcio_4
LIBncep       = ../nwprod/lib
SHTNSLIB	= ../lib
OMPFLAG = -qopenmp


.SUFFIXES: .o .f90 .F90

all: psop psop2

OBJS_GRDTOSPEC=kinds.o specmod.o rnorm.o constants.o stop1.o mpisetup.o

psop: $(OBJS_GRDTOSPEC) 
	$(FCS) $(FFLAGS) $(FREEFLAG) $(OMPFLAG) -I$(INCsigio) -o psop_gfs_biascor.x psop_gfs.f90 $(OBJS_GRDTOSPEC) -L$(SHTNSLIB) -lshtns -L$(FFTW_DIR) -lfftw3_omp -lfftw3  -L$(LIBncep) -lsigio_4 
 
psop2: $(OBJS_GRDTOSPEC) 
	$(FCS) $(FFLAGS) $(FREEFLAG) $(OMPFLAG) -I$(INCsigio) -o psop_gfs_biascor2be.x psop_gfs_test.f90 $(OBJS_GRDTOSPEC) -L$(SHTNSLIB) -lshtns -L$(FFTW_DIR) -lfftw3_omp -lfftw3 -L$(LIBncep) -lsigio_4 

psop3: $(OBJS_GRDTOSPEC) 
	$(FCS) $(FFLAGS) $(FREEFLAG) $(OMPFLAG) -I$(INCsigio) -o psop_gfs_be.x psop_gfs_be.f90 $(OBJS_GRDTOSPEC) -L$(SHTNSLIB) -lshtns -L$(FFTW_DIR) -lfftw3_omp -lfftw3 -L$(LIBncep) -lsigio_4 

clean:
	rm -f $(OBJS_GETSIGENSMEAN) $(OBJS_GRDTOSPEC) *.mod *.x *.o $(EXEC) loadmap.txt

.F90.o:
	$(FCP) $(FFLAGS) -c $<

.f90.o:
	$(FCP) $(FFLAGS) -c $< 

.f.o:
	$(FCP) $(FFLAGS) -free -c $< 

%.o : %.mod

kinds.o:	kinds.f90

constants.o:	constants.f90 kinds.o

specmod.o:	specmod.f90 kinds.o

rnorm.o:	rnorm.f90 kinds.o

stop1.o:	stop1.f90 kinds.o mpisetup.o

mpisetup.o:	mpisetup.f90
