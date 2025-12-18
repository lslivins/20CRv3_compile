#!/bin/ksh
set -x

export ESMF_DIR=${ESMF_DIR:-$(pwd)}
cd $ESMF_DIR
export HOMEDIR=${HOMEDIR:-${NWPRODLIB:-../../..}}
cd $HOMEDIR
echo $(pwd)
LIBDIR=$(pwd)
cd $ESMF_DIR

#export ESMF_BOPT=O
#unset ESMF_ARCH ESMF_PREC
#
export ESMF_INSTALL_PREFIX=$HOME/w/e5t
mkdir -p ESMF_INSTALL_PREFIX
export ESMF_COMM=mpich2
export ESMF_COMM=mpi
export ESMF_COMPILER=intel
export ESMF_MACHINE=x86_64
export ESMF_OS=Unicos
export ESMF_ABI=64
export ESMF_BOPT=O
# The ON setting here was being overwritten to OFF by ./build_config/Unicos.intel.default/build_rules.mk
#export ESMF_PTHREADS=ON
export ESMF_PTHREADS=OFF
export ESMF_TESTWITHTHREADS=OFF

gmake clobber
gmake
cd $ESMF_DIR/lib/lib$ESMF_BOPT/Unicos.intel.64.mpi.default
echo $(pwd)
mv libesmf.a $LIBDIR/libesmf_3_1_0rp5.a
mkdir $LIBDIR/incmod/esmf_3_1_0rp5
cd $ESMF_DIR/mod/mod$ESMF_BOPT/Unicos.intel.64.mpi.default
echo $(pwd)
mv * $LIBDIR/incmod/esmf_3_1_0rp5
cd $ESMF_DIR
#gmake clobber  # to clean up the unnecessary files in the src directory
