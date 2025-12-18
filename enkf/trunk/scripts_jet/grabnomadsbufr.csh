set date=$1
set RUN=$2 
set WORK=/lfs1/projects/fim/whitaker
set datem1 = `$HOME/bin/incdate $date -6`
set hr=`echo $date | cut -c9-10`
set hrm1=`echo $datem1 | cut -c9-10`
set YYYYMMDD=`echo $date | cut -c1-8`
set YYYYMMDDm1=`echo $datem1 | cut -c1-8`
set types = "1bamua 1bamub 1bhrs3 1bhrs4 1bmhs amsre airsev osbuv8 ssmisu gpsro sptrmm trmm mtiasi goesfv omi gome esamua esamub eshrs3"
set outdir=bufr_${date}
/bin/rm -rf $outdir
mkdir -p $outdir
cd $outdir
if ($RUN == 'gfs') then
   set url = http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.${date}
else
   set url = http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gdas.${YYYYMMDD}
endif
/usr/bin/wget ${url}/${RUN}.t${hr}z.prepbufr.unblok.nr  
if ($status != 0) exit 1
$WORK/gfsenkf/cwordsh.fd/cwordsh block ${RUN}.t${hr}z.prepbufr.unblok.nr ${RUN}.t${hr}z.prepbufr
if ($status != 0) exit 1
/bin/rm -f  ${RUN}.t${hr}z.prepbufr.unblok.nr
foreach type ($types)
  /usr/bin/wget ${url}/${RUN}.t${hr}z.${type}.tm00.bufr_d
  if ($status == 0) then
     /bin/mv -f ${RUN}.t${hr}z.${type}.tm00.bufr_d ${RUN}.t${hr}z.${type}.tm00.bufr_d.unblok.nr
     $WORK/gfsenkf/cwordsh.fd/cwordsh block ${RUN}.t${hr}z.${type}.tm00.bufr_d.unblok.nr ${RUN}.t${hr}z.${type}.tm00.bufr_d
     /bin/rm -f ${RUN}.t${hr}z.${type}.tm00.bufr_d.unblok.nr
  endif
end

# get grib files.
set cnvgrib = /opt/cnvgrib/cnvgrib-1.1.5/bin/cnvgrib
set types = "sstgrb snogrb engicegrb"
foreach type ($types)
   set gdasurl = http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gdas.${YYYYMMDD}
   set grib2file=${gdasurl}/gdas1.t${hr}z.${type}.grib2
   /usr/bin/wget ${gdasurl}/${grib2file}
   if ($status != 0) then # try file from previous time.
      set gdasurl = http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gdas.${YYYYMMDDm1}
      set grib2file=gdas1.t${hrm1}z.${type}.grib2
      /usr/bin/wget ${gdasurl}/${grib2file}
      if ($status != 0) exit 1
   endif
   $cnvgrib -g21 $grib2file ${RUN}.t${hr}z.${type}
   if ($status != 0) exit 1
   /bin/rm -f $grib2file
end
cd ..
/bin/ls -l bufr_${date}
