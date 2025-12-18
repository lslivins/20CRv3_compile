#!/bin/sh
set -x
export LIBS=../..
mkdir $LIBS/incmod/nemsio
export FCMP=${1:-ftn}
if [ $FCMP = xlf95_r ] ; then
 export FFLAGSM="-O3"
 export AR=ar
 export ARFLAGS="-rv -X64"
else
 export FFLAGSM="-O3 -FR -convert big_endian -traceback -I$(INCMOD)"
 export FFLAGBM="-O3 -convert big_endian -traceback"
 export AR=ar
 export ARFLAGS=-ruv
fi
make -f Makefile clean
make -f Makefile
