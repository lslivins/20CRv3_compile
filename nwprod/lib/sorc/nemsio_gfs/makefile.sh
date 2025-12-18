#!/bin/sh
set -x
export LIBDIR=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib
export FCMP=${1:-ftn}
if [ $FCMP = xlf_r ] ; then
 export FFLAGSM="-O3  -q free -I /nwprod/lib/incmod/nemsio"
 export ARFLAGSM="-rv X64"
else
 export FFLAGSM="-O2 -g -traceback -convert big_endian -FR -I/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib/incmod/nemsio"
 export ARFLAGSM="-rv"
fi
mkdir -p $LIBDIR/incmod/nemsio_gfs
#make -f Makefile clean
make -f Makefile
