#!/bin/csh
cd $datapath2/ens${nanals2}
/bin/rm -f enspost*
set tmp_grib_file="enspost_tmp$$"
set fhr=0
while ($fhr <= $FHMAX)
    set charfhr=`printf %02i $fhr`
    set nanal=1
    while ($nanal <= $ENS_NUM)
       set charnanal=`printf %03i $nanal`

       set grib_file=pgrbfg_${analdate}_fhr${charfhr}_mem${charnanal}

       set field=HGT
       set level=500 
       set new_grib_file="enspost_${analdate}.z${level}"
       wgrib -s $grib_file | grep ":${field}:${level} mb:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set level=1000
       set new_grib_file="enspost_${analdate}.z${level}"
       wgrib -s $grib_file | grep ":${field}:${level} mb:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set field=TMP
       set level=850 
       set new_grib_file="enspost_${analdate}.t${level}"
       wgrib -s $grib_file | grep ":${field}:${level} mb:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set field=UGRD
       set level=850 
       set new_grib_file="enspost_${analdate}.u${level}"
       wgrib -s $grib_file | grep ":${field}:${level} mb:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set level=200 
       set new_grib_file="enspost_${analdate}.u${level}"
       wgrib -s $grib_file | grep ":${field}:${level} mb:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set field=VGRD
       set level=850 
       set new_grib_file="enspost_${analdate}.v${level}"
       wgrib -s $grib_file | grep ":${field}:${level} mb:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set level=200 
       set new_grib_file="enspost_${analdate}.v${level}"
       wgrib -s $grib_file | grep ":${field}:${level} mb:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set field=UGRD
       set level=10 
       set new_grib_file="enspost_${analdate}.u${level}m"
       wgrib -s $grib_file | grep ":${field}:${level} m above gnd:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set field=VGRD
       set level=10 
       set new_grib_file="enspost_${analdate}.v${level}m"
       wgrib -s $grib_file | grep ":${field}:${level} m above gnd:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set field=TMP
       set level=2 
       set new_grib_file="enspost_${analdate}.t${level}m"
       wgrib -s $grib_file | grep ":${field}:${level} m above gnd:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       set field=PRMSL
       set new_grib_file="enspost_${analdate}.prmsl"
       wgrib -s $grib_file | grep ":${field}:" | wgrib -i $grib_file -grib -o $tmp_grib_file
       cat $tmp_grib_file >> $new_grib_file

       @ nanal = $nanal + 1
    end
    @ fhr = $fhr + $FHOUT
end
/bin/rm -f $tmp_grib_file
exit 0
