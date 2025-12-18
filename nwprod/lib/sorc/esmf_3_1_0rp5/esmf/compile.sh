#!/bin/ksh
set -x

export ESMF_DIR=${ESMF_DIR:-$(pwd)}
cd $ESMF_DIR
export HOMEDIR=${HOMEDIR:-${NWPRODLIB:-../../..}}
cd $HOMEDIR
echo $(pwd)
LIBDIR=$(pwd)
cd $ESMF_DIR
export ESMF_BOPT=O
unset ESMF_ARCH ESMF_PREC
gmake clobber
gmake
cd $ESMF_DIR/lib/libO/Linux.intel.64.mpiuni.default
mv libesmf.a $LIBDIR/libesmf_3_1_0rp5.a
mkdir $LIBDIR/incmod/esmf_3_1_0rp5
cd $ESMF_DIR/mod/modO/Linux.intel.64.mpiuni.default
mv * $LIBDIR/incmod/esmf_3_1_0rp5
cd $ESMF_DIR
#gmake clobber  # to clean up the unnecessary files in the src directory
