#PBS -A cpo_ngrr_e
#PBS -l partition=es,size=1,walltime=16:00:00
#PBS -q rdtn
#PBS -N gfsenkf_hpss 
#PBS -e hpss.err
#PBS -o hpss.out
#PBS -S /bin/csh
module load hsi
cd /lustre/f1/unswept/Jeffrey.S.Whitaker/gfsenkf_20crV3_cmip5oz
set date=2008090106
while ($date <= 2009010100)
  htar -cvf /ESRL/BMC/gsienkf/2year/gfsenkf_20crV3_cmip5oz/${date}.tar ${date}
  set date=`incdate $date 6`
end
hsi ls -l /ESRL/BMC/gsienkf/2year/gfsenkf_20crV3_cmip5oz
