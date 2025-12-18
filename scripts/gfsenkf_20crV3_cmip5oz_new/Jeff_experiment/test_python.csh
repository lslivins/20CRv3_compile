#!/bin/csh
#PBS -q batch
#PBS -A cpo_ngrr_e
#PBS -l partition=c3
#PBS -l size=1 
#PBS -l walltime=00:30:00
#PBS -q debug
#PBS -N test_python  
#PBS -e test_python.err
#PBS -o test_python.out
#PBS -S /bin/csh
module use /sw/eslogin-c3/modulefiles
module switch intel intel/14.0.2.144
/lustre/f1/unswept/Jeffrey.S.Whitaker/20CRV3/bin/global_sighdr /lustre/f1/unswept/Jeffrey.S.Whitaker//gfsenkf_20crV3_cmip5oz//2005080400//sfg_2005080400_fhr03 idvc
