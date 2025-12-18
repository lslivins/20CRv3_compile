export SIGIO_INC4=/scratch3/NCEPDEV/nwprod/lib/sigio/v1.0.1/incmod/sigio_v1.0.1_4
export SIGIO_LIB4=/scratch3/NCEPDEV//nwprod/lib/sigio/v1.0.1/libsigio_v1.0.1_4.a
sh -x make_w3emc_lib.sh ifort.setup 

cp -r w3emc ../../../w3emc

