#!/bin/csh
#module switch PrgEnv-cray PrgEnv-intel
#module load cray-netcdf
#module load cray-hdf5

#module switch cray-mpich cray-mpich/7.0.4
#module switch cray-shmem cray-shmem/7.0.4

### SEG - convert enviro to intel
#   SEG  enter module switches before build.csh
module load PrgEnv-cray  # seg
module switch PrgEnv-cray PrgEnv-intel

#+ SEG Block
module load  intel/2023.2.0
module load craype 
module load cray-hdf5/1.12.2.9
module load cray-netcdf/4.9.0.9
#- 
### SEG - swap chips
#module swap craype-haswell craype-mic-knl
#module load craype-mic-knl
# suggested by Jack Deslippe, along with -lmemkind to the gfs

#module load cray-memkind

#module switch intel intel/16.0.3.210 
# turned this back on now (20 October 2019) that intel/19.0.3.199 is the default
#module switch intel intel/17.0.1.132
# turned this back off (21 October 2019) use the default intel/19.0.3.199
# ensure use of intel/19.0.3.199 for testing (28 June 2022)

# SEG Load intel
# module switch intel intel/19.0.3.199  # seg
#module load cray-netcdf
#module load cray-hdf5
# as of 10 Nov no fftw_omp on default
# SEG problems - load cray-fftw later (after build)
module unload fftw

# T. Kurth recommend craype-hugepages2M module
#module load craype-hugepages2M

# SEG tracker
#module unload darshan
# to get performance information from Cray
# without rewriting code or changing compiler options
# use CrayPat-Lite
#module load perftools-base
#module load perftools-lite

# 24 October 2019 do not use the NERSC wrapped gcc - made system slower by factor of 2 
#module load gcc
echo "building esmf"
# SEG pushd cd dir; exec
# SEG pop cd back
# This compiled OK 4/3
pushd nwprod/lib/sorc/esmf_3_1_0rp5/esmf; sh compile_intel_unicos.sh
popd

set dothis="False"
set dothis="True"
if $dothis == "True" then
   # nwprod - numerical weather products
   module load cray-fftw
   echo "building bacio"
   pushd nwprod/lib/sorc/bacio; /bin/rm -f ../../libbacio*a;  make -f makefile4
   popd
   
   
   
   module load cray-fftw
   echo "build sp"
   pushd nwprod/lib/sorc/sp; sh makelibsp.sh
   popd
   
   echo "build w3lib"
   pushd nwprod/lib/sorc/w3lib-2.0; /bin/rm -f ../../libw3*a; make -f makefile_4 clean; make -f makefile_4; make -f makefile_d clean; make -f makefile_d
   cd ../..
   ln -fs libw3-2.0_4.a libw3lib-2.0_4.a
   ln -fs libw3-2.0_d.a libw3lib-2.0_d.a
   popd
   
   # vertical levels - though now hybrid-sigma
   echo "build sigio"
   pushd nwprod/lib/sorc/sigio; /bin/rm -f ../../libsigio*.a;   sh makefile.sh
   popd
   
   # IO surface fluxes
   echo "build sfcio"
   pushd nwprod/lib/sorc/sfcio;  /bin/rm -f ../../libsfcio*.a; sh makefile.sh
   popd
   
   echo "build ip"
   pushd nwprod/lib/sorc/ip; make clean; make; /bin/cp -f libip_4.a ../../; make -f Makefile_d clean; make -f Makefile_d; /bin/cp -f libip_d.a ../..
   popd
   
   echo "build nemsio"
   pushd nwprod/lib/sorc/nemsio; sh makefile.sh
   popd
   
   # 2017 NOAA model - exp versions with extar bits Geof/Jeff Whittiker(?) still supports
   # Not standard prognostic ozone, but read in your own (standard is recent, with CFCs)
   echo "build gfs"
   pushd gfs/global_fcst.fd.v14.0.1 ; make clean; sh makefile.sh; /bin/cp -f global_fcst_cmip5 ../../bin
   popd
   
   # SST/ice   parallel version may be an issue - use nnon-parallel
   echo "build global_cycle"
   pushd gfs/global_cycle.fd; /bin/rm -f *.o *.mod global_cycle*; make -f Makefile; make -f Makefilep
   popd
   
   # buiilding output GRIB
   echo "build global_postgp"
   pushd gfs/global_postgp.fd; /bin/rm -f global_postgp* *.o *.mod; make -f Makefile; make -f Makefile_p; /bin/cp -f global_postgp* ../../bin
   popd
   
   echo "build global_sfchdr"
   pushd gfs/global_sfchdr.fd; /bin/rm -f global_sfchdr; make; /bin/cp -f global_sfchdr ../../bin
   popd
   
   echo "build global_sighdr"
   pushd gfs/global_sighdr.fd; /bin/rm -f global_sighdr; make; /bin/cp -f global_sighdr ../../bin
   popd
   
   # Non NCEP local routines. Only interested in model datat wrt oberveed pressure (ony obs used)
   echo "build psop"
   pushd psop; make clean; make; /bin/cp -f *.x ../bin
   popd
   
   echo "build enkf"
   pushd enkf/trunk/src; make clean; make
   popd
   
   echo "build enkf utilities"
   pushd enkf/trunk/util/src; /bin/rm -f *.x *.o *.mod; make -f Makefile_gfs_nersc; /bin/cp -f *.x ../../../../bin
   popd
   
   # Switch to GNU for this bit
   module swap PrgEnv-intel PrgEnv-gnu
   
   echo "build h5 reader"
   pushd h5f_reader; make clean; make; /bin/cp -f h5ftotxt ../bin
   popd
   
   echo "build h5totxt_v4 reader"
   pushd h5f_reader_v4; make clean; make; /bin/cp -f h5totxt_v4 ../bin
   popd
   
   # Switch back to Intel
   module swap PrgEnv-gnu PrgEnv-intel
   
   echo "build gfsio"
   pushd nwprod/lib/sorc/gfsio; /bin/rm -f ../../libgfsio*a; make -f makefile_4
   popd
   
   echo "build landsfcutil"
   pushd nwprod/lib/sorc/landsfcutil; sh makefile_d
   popd
   
   echo "build w3emc"
   pushd nwprod/lib/sorc/w3emc_v2.2.0/sorc; make clean; make ; /bin/cp -f w3emc/v2.2.0/libw3emc*a ../../../; /bin/rm -rf ../../../incmod/w3emc*; /bin/cp -R w3emc/v2.2.0/incmod/* ../../../incmod
   popd
   
   echo "build w3nco"
   pushd nwprod/lib/sorc/w3nco_v2.0.6; sh makelibw3_nco.sh
   popd
   
   echo "build bacio_v2.0.1"
   pushd nwprod/lib/sorc/bacio_v2.0.1/src; sh makebacio_nersc.sh
   popd
   
   echo "build gfsio_v1.1.0"
   pushd nwprod/lib/sorc/gfsio_v1.1.0; /bin/rm -f ../../libgfsio*a; make -f makefile_4
   popd
   
   # build everything else for haswell
   # Back to haswell??!?!  (SEG)
   module swap craype-mic-knl craype-haswell
   
   echo "build sig2grb"
   pushd sig2grb; make clean; make; /bin/cp -f sig2grb ../bin
   popd
   
   echo "build global_chgres"
   pushd gfs/global_chgres.fd; make clean
   sh makefile.sh
   popd
else
   echo "Skipping some stuff"
endif  
