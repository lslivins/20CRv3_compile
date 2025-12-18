#!/bin/sh
###############################################################
#
#   AUTHOR:    Vuong - W/NP11
#
#   DATE:      12/04/2000
#
#   PURPOSE:   This script uses the make utility to update the libsp 
#              archive libraries.
#              It first reads a list of source files in the library and
#              then generates a makefile used to update the archive
#              libraries.  The make command is then executed for each
#              archive library, where the archive library name and 
#              compilation flags are passed to the makefile through 
#              environment variables.
#
#   REMARKS:   Only source files that have been modified since the last
#              library update are recompiled and replaced in the object
#              archive libraries.  The make utility determines this
#              from the file modification times.
#
#              New source files are also compiled and added to the object 
#              archive libraries.
#
###############################################################

#-------------------------------------------------------------------------------
#     Determine the byte-ordering scheme used by the local machine.
set -x

cat > endiantest.c << ENDIANTEST

#define Order(x)\
        fill((char *)&x, sizeof(x)); \
        for (i=1; i<=sizeof(x); i++) { \
            c=((x>>(byte_size*(sizeof(x)-i)))&mask); \
            putchar(c==0 ? '?' : (char)c); \
        } \
        printf("\n");

void fill(p, size) char *p; int size; {
        char *ab= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int i;

        for (i=0; i<size; i++) p[i]= ab[i];
}

void endian(byte_size) int byte_size; {
        int j=0;
        unsigned int mask, i, c;

        mask=0;
        for (i=1; i<=(unsigned)byte_size; i++) mask= (mask<<1)|1;
        Order(j);
}
int cprop() {
        /* Properties of type char */
        char c;
        int byte_size;

        c=1; byte_size=0;
        do { c<<=1; byte_size++; } while(c!=0);

        return byte_size;
}

main()
{
        int byte_size;

        byte_size= cprop();
        endian(byte_size);
}
ENDIANTEST

export FCMP=${1:-${FCMP:-xlf_r}}
export CCMP=${2:-${CCMP:-xlc_r}}

$CCMP -o endiantest endiantest.c

if [ $(./endiantest | cut -c1) = A ] ; then
    byte_order=BIG_ENDIAN
else
    byte_order=LITTLE_ENDIAN
fi

rm -f endiantest.c endiantest

#--------------------------------------------------------------
#      Preprocess any Fortran *.F files into corresponding *.f files
export CPP=${3:-${CPP:-/usr/bin/cpp}}

BFNS=""

for i in `ls *.F` ; do
  bn=`basename $i .F`
  bnf=${bn}.f
  BNFS="$BNFS $bnf"
  $CPP -traditional -P -D$byte_order $i $bnf
done

#
#     Generate a list of object files that corresponds to the
#     list of Fortran ( .f ) files in the current directory
#
for i in `ls *.f` ; do
  obj=`basename $i .f`
  OBJS="$OBJS ${obj}.o"
done

#
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
#
if [ -f make.gfsio ] ; then
  rm -f make.gfsio
fi
#
#     Generate a new make file ( make.libsp), with the updated object list,
#     from this HERE file.
#
cat > make.gfsio << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )
\$(LIB)(bafrio.o):    bafrio.f
	\$(FCMP) -c \$(FFLAGB) \$<
	ar -ruv \$(AFLAGB)  \$@ \$*.o
	rm -f \$*.o

.f.a:
	\$(FCMP) -c \$(FFLAGS) \$<
	ar -ruv \$(AFLAGS)  \$@ \$*.o
	rm -f \$*.o
	mv *.mod \$(INCMOD)

EOF

export LIB="../../libgfsio_4.a"
export INCMOD="../../incmod/gfsio_4"
mkdir -p $INCMOD
if [ $FCMP = xlf_r ] ; then
 export FFLAGS="-qnosave -O3 -q free=f90 -I\$(INCMOD)"
 export FFLAGB="-qnosave -O3 -q fixed"
else
 export FFLAGS="-O3 -xHOST -convert big_endian -traceback -FR -I\$(INCMOD)"
 export FFLAGB="-O3 -xHOST -convert big_endian -traceback"
fi
make -f make.gfsio

export LIB="../../libgfsio_d.a"
export INCMOD="../../incmod/gfsio_d"
mkdir -p $INCMOD
if [ $FCMP = xlf_r ] ; then
 export FFLAGS="-qnosave -O3 -q realsize=8 -q free=f90 -I\$(INCMOD)"
 export FFLAGB="-qnosave -O3 q realsize=8 -q fixed"
else
 export FFLAGS="-O3 -xHOST -r8 -convert big_endian -traceback -FR -I\$(INCMOD)"
 export FFLAGB="-O3 -xHOST -r8 -convert big_endian -traceback"
fi
make -f make.gfsio

rm -f make.gfsio $BNFS
