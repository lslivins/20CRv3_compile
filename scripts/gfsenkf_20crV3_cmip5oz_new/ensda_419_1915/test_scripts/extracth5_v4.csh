#module load cray-hdf5
set date=$1
set fileout=$2
/bin/rm -f $fileout
touch $fileout
$HOMEGLOBAL/h5f_reader_v4/h5totxt_v4 $date >> $fileout
#$HOMEGLOBAL/bin/h5totxt_v4 $date >> $fileout
