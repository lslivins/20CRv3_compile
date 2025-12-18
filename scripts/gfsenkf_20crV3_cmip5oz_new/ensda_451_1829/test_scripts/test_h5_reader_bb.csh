#!/bin/csh
# Testing the extract h5 script reading from BB

# need to run interactively on KNL
# salloc -N 1 -t 20:00 -C knl,quad,cache --qos=interactive --bbf=bbf.conf
#
# or on Haswell
#
# salloc -N 1 -t 30:00 -C haswell --qos=interactive --bbf=bbf.conf
# then
# date; csh ./test_h5_reader_bb.csh ; echo "out" ; date


echo "inside test script"
#module load cray-netcdf/4.4.0
#module load cray-hdf5


#module load cray-hdf5

date
set analdate=1915090812

echo $analdate

# if you are on Haswell
#setenv basedir /global/cscratch1/sd/compo/20CRV3_CoriI_stripe4/

# if you are on KNL uncomment this one and comment the CoriI basedir above
setenv basedir /global/cscratch1/sd/compo/20CRV3_CoriII_stripe4/
echo $basedir 

setenv HOMEGLOBAL ${basedir}
setenv EXECGLOBAL ${basedir}/bin 

echo $EXECGLOBAL


#setenv obsdirh5 /global/cscratch1/sd/lslivins/HDF5/V41_striped/
# try a stripe = 1 location
setenv obsdirh5 /global/cscratch1/sd/cmccoll/HDF5/V46/
#setenv obsdirh5 `echo $DW_PERSISTENT_STRIPED_V46`

echo $obsdirh5

setenv enkfscripts $PWD
echo $enkfscripts

setenv incdate "${enkfscripts}/incdate"

set outfile=$DW_PERSISTENT_STRIPED_V46/outfiles/psobfile_$analdate.txt
echo "writing to"
echo $outfile
echo " "
echo "starting extraction"
date
csh ${enkfscripts}/extracth5_v4.csh $analdate $outfile
date
echo "done"

head -n 5 $outfile

