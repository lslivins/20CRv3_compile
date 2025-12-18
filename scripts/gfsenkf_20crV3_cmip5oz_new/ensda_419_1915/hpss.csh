#PBS -d /lustre/f1/unswept/Gilbert.P.Compo/scripts/gfsenkf_20crV3_cmip5oz/ensda_425_1915
#PBS -A cpo_ngrr_e
#PBS -l partition=es,size=1,walltime=04:00:00
#PBS -q rdtn
#PBS -N 425_1915_hpss
#PBS -e hpss.err
#PBS -o hpss.out
#PBS -S /bin/csh

setenv PATH /home/Gilbert.P.Compo/bin:$PATH
module load hsi
source $MODULESHOME/init/csh
setenv hpss_date "hpss_date.csh"
source $hpss_date

cd /lustre/f1/unswept/Gilbert.P.Compo/gfsenkf_20crV3_cmip5oz/ensda_425_1915/
htar -cvf /ESRL/BMC/gsienkf/5year/Gilbert.P.Compo/gfsenkf_20crV3_cmip5oz/ensda_425_1915/${hpss_date}.tar ${hpss_date}
#end
