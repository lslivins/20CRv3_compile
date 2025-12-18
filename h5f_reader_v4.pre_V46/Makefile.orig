# If you run make -k with no other arguments, then it will try to make the
# first target, which I call all.  This will force both executables
#	module switch PrgEnv-pgi PrgEnv-intel
#   	module load hdf5/1.8.5.0
#	setenv PERL5LIB ~/h5f_reader
all:	h5totxt_v4

## software can be downloaded from http://www.hdfgroup.org/downloads/index.html
#CC = /home/cmccoll/hdf5-1.6.10/tools/misc/h5cc
#CC = /usr/local/bin/h5cc
#CC = /usr/localc/gcc-4.7/bin/h5cc
## would work but no perl Date::Manip CC = /opt/cray/hdf5/1.8.5.0/hdf5-intel/bin/h5cc
CC =  gcc -O2

h5totxt_v4: ispdb2.o nmtools.o h5totxt_v4.o
	${CC} ${DEBUG_FLAG} ${OPT_FLAG} -I${HDF5_DIR}/include  ispdb2.o nmtools.o h5totxt_v4.o -lm -L${HDF5_DIR}/lib -lhdf5 -lhdf5_hl -o h5totxt_v4

nmtools.o: nmtools.h nmtools.c
	${CC} ${DEBUG_FLAG} ${OPT_FLAG} -I${HDF5_DIR}/include -c nmtools.c
ispdb2.o: ispdb_data.h ispdb2.h ispdb2.c
	${CC} ${DEBUG_FLAG} ${OPT_FLAG} -I${HDF5_DIR}/include -c ispdb2.c

h5totxt_v4.o: ispdb_data.h ispdb2.h  nmtools.h h5totxt_v4.c
	${CC} ${DEBUG_FLAG} ${OPT_FLAG} -I${HDF5_DIR}/include -c h5totxt_v4.c

# Running make -k clean will remove all the files that this makefile
# is capable of making.  It uses the rm command for both Unix and
# Windows, assuming that the cs1300 software is installed (because
# that software includes the rm command).  The -f option says to
# force removal of a file, even if it is read-only.
clean:
	rm -f h5totxt_v4 
	rm -f nmtools.o ispdb2.o h5totxt_v4.o 
