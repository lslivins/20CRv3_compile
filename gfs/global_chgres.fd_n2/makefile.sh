#!/bin/sh
set -x
#export LIBDIR=../../lib
export LIBDIR=../../lib_test2
#export FCMP=xlf_r
#export FCMP95=xlf95_r
export FCMP=${1:-ftn}
export FCMP95=${2:-${FCMP}}
if [ $FCMP = xlf_r ] ; then
 export FFLAGSM=" -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1"
 export FFLAGS2M=" -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1"
 export LIBFLAGSM="-lessl "
 export OMPFLAGM=
 export RECURS=
else
#export FFLAGSM=" -O0 -r8 -xHOST -convert big_endian -traceback -override-limits -g"
#export FFLAGS2M=" -O0 -r8 -xHOST -convert big_endian -traceback -override-limits -g -FR"

 export FFLAGSM=" -g -O3 -r8 -xHOST -convert big_endian -traceback"
 export FFLAGS2M=" -g -O3 -r8 -xHOST -convert big_endian -traceback -FR"

#export RECURS="-recursive"
 export RECURS=

 export LIBFLAGSM=" "
 export LDFLAGSM=-mkl
 export OMPFLAGM=-openmp
fi
make -f Makefile
#make -f Makefile2
