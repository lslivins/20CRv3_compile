#!/bin/csh
# Testing the extract h5 script

# need to run interactively on KNL
# salloc -p debug -t 30:00 -C knl,quad,cache
# or
# salloc -p debug -t 30:00 -C haswell
# then
# date; csh ./test_h5_reader.csh ; echo "out" ; date

echo "inside test script"
#module load cray-hdf5

date
set analdate=1910070500

echo $analdate

setenv basedir /global/cscratch1/sd/compo/20CRV3_CoriII_stripe4/
echo $basedir 

setenv HOMEGLOBAL ${basedir}
setenv EXECGLOBAL ${basedir}/bin 

echo $EXECGLOBAL


#setenv obsdirh5 /global/cscratch1/sd/lslivins/HDF5/V41_striped/
# try a stripe = 1 location
#setenv obsdirh5 /global/cscratch1/sd/compo/HDF5/V41/
setenv obsdirh5 /global/cscratch1/sd/cmccoll/HDF5/V46/

echo $obsdirh5

setenv enkfscripts $PWD
echo $enkfscripts

setenv incdate "${enkfscripts}/incdate"

set outfile=$CSCRATCH/psobfile_$$.txt
echo "writing to"
echo $outfile
echo " "
echo "starting extraction"
date
csh ${enkfscripts}/extracth5_v4.csh $analdate $outfile
date
echo "done"

