#!/bin/ksh
set -x

mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)

if [ $mac2 = tf ]; then
machine=theia
elif [ $mac = t -o $mac = g ]; then 
machine=wcoss
elif [ $mac = f ]; then
machine=zeus
else
 echo "Machine Option Not Found, exit"
 exit
fi

#---------------------------------------------
if [ $machine = zeus ]; then #For Zeus
#---------------------------------------------

export NWPRODLIB=/contrib/nceplibs/nwprod/lib
export FCMP=ifort

export W3NCO_VER=v2.0.6
export W3NCO_DIR=$NWPRODLIB
export W3NCO_LIBd=w3nco_${W3NCO_VER}_d

export W3EMC_VER=v2.0.5
export W3EMC_DIR=$NWPRODLIB
export W3EMC_LIBd=w3emc_${W3EMC_VER}_d

export SP_VER=v2.0.1
export SP_DIR=$NWPRODLIB
export SP_LIBd=sp_${SP_VER}_d

export SFCIO_VER=
export SFCIO_DIR=$NWPRODLIB
export SFCIO_LIB4=sfcio_4
export SFCIO_INC4=${SFCIO_DIR}/incmod/$SFCIO_LIB4

export BACIO_VER=v2.0.1
export BACIO_DIR=$NWPRODLIB
export BACIO_LIB4=bacio_${BACIO_VER}_4

#---------------------------------------------
elif [ $machine = theia ] ; then  #For Theia
#---------------------------------------------

export NWPRODLIB=/scratch3/NCEPDEV/nwprod/lib
export FCMP=mpiifort

export W3NCO_VER=v2.0.6
export W3NCO_DIR=$NWPRODLIB
export W3NCO_LIBd=w3nco_${W3NCO_VER}_d

export W3EMC_VER=v2.0.5
export W3EMC_DIR=$NWPRODLIB
export W3EMC_LIBd=w3emc_${W3EMC_VER}_d

export SP_VER=v2.0.1
export SP_DIR=$NWPRODLIB
export SP_LIBd=sp_${SP_VER}_d

export SFCIO_VER=
export SFCIO_DIR=$NWPRODLIB
export SFCIO_LIB4=sfcio_4
export SFCIO_INC4=${SFCIO_DIR}/incmod/$SFCIO_LIB4

export BACIO_VER=v2.0.1
export BACIO_DIR=$NWPRODLIB
export BACIO_LIB4=bacio_${BACIO_VER}_4

#---------------------------------------------
elif [ $machine = wcoss  ] ; then  #For WCOSS
#---------------------------------------------

export NWPRODLIB=/nwprod/lib
export FCMP=ifort

export W3NCO_VER=v2.0.6
export W3NCO_DIR=$NWPRODLIB/w3nco/$W3NCO_VER
export W3NCO_LIBd=w3nco_${W3NCO_VER}_d

export W3EMC_VER=v2.0.5
export W3EMC_DIR=$NWPRODLIB/w3emc/$W3EMC_VER
export W3EMC_LIBd=w3emc_${W3EMC_VER}_d

export SP_VER=v2.0.2
export SP_DIR=$NWPRODLIB/sp/$SP_VER
export SP_LIBd=sp_${SP_VER}_d

export SFCIO_VER=v1.0.0
export SFCIO_DIR=$NWPRODLIB/sfcio/$SFCIO_VER
export SFCIO_LIB4=sfcio_${SFCIO_VER}_4
export SFCIO_INC4=${SFCIO_DIR}/incmod/$SFCIO_LIB4

export BACIO_VER=v2.0.1
export BACIO_DIR=$NWPRODLIB/bacio/$BACIO_VER
export BACIO_LIB4=bacio_${BACIO_VER}_4

#---------------------------------------------
fi
#---------------------------------------------


##export DEBUG='-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector -traceback -g'
export FFLAGS="-O3 -r8 -convert big_endian -traceback -g"
export OMPFLAG=-openmp
export LDFLG=-openmp

if [ ! -d ../../exec/ ] ; then
  mkdir -p ../../exec
fi

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean
