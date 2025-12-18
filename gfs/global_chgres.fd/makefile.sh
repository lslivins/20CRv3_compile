#!/bin/ksh
set -x

export NWPRODLIB=../../nwprod/lib
export FCMP=ftn
export FCMP95=$FCMP

export W3NCO_DIR=$NWPRODLIB
export W3NCO_LIBd=w3nco_d

export W3EMC_DIR=$NWPRODLIB
export W3EMC_LIBd=w3emc_v2.2.0_d

export SP_DIR=$NWPRODLIB
export SP_LIBd=sp_d

export IP_DIR=$NWPRODLIB
export IP_LIBd=ip_d

export SFCIO_DIR=$NWPRODLIB
export SFCIO_LIB4=sfcio_4
export SFCIO_INC4=${SFCIO_DIR}/incmod/$SFCIO_LIB4

export SIGIO_DIR=$NWPRODLIB
export SIGIO_LIB4=sigio_4
export SIGIO_INC4=${SIGIO_DIR}/incmod/$SIGIO_LIB4

export GFSIO_DIR=$NWPRODLIB
export GFSIO_LIB4=gfsio_4
export GFSIO_INC4=${GFSIO_DIR}/incmod/$GFSIO_LIB4

export NEMSIO_DIR=$NWPRODLIB
export NEMSIO_LIB=nemsio
export NEMSIO_INC=${NEMSIO_DIR}/incmod/$NEMSIO_LIB

export LANDSFCUTIL_DIR=$NWPRODLIB
export LANDSFCUTIL_LIBd=landsfcutil_d
export LANDSFCUTIL_INCd=${LANDSFCUTIL_DIR}/incmod/$LANDSFCUTIL_LIBd

export BACIO_DIR=$NWPRODLIB/sorc/bacio_v2.0.1/lib
export BACIO_LIB4=bacio_4

export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
export FFLAGS2M="-i4 -O3 -r8 -convert big_endian -fp-model precise -FR"
#export RECURS="-recursive"
export RECURS=
export LDFLAGSM="-qopenmp -auto"
export OMPFLAGM="-qopenmp -auto"

make -f Makefile
#make -f Makefile install
#make -f Makefile clean
