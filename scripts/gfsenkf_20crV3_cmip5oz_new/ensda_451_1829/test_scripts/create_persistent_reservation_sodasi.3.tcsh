#!/bin/tcsh


#### Which partition? 
#SBATCH -p regular

#### which account
#SBATCH -A m2902

#### how many nodes
#SBATCH -N 1

#### how much time
#SBATCH -t 00:30:00

#### give the job a name
#SBATCH -J create_sodasi

#### where does standard error and standard out go? 
#SBATCH -e create_soda.err
#SBATCH -o create_sodasi.out

##### is this for Haswell or KNL
#SBATCH -C knl,cache,quad

#### knl has 68 cpus, only using up to 65 MPI ranks
## so, give 2 for the OS
#SBATCH -S 2

#### make the job depend on cscratch1 being available
#SBATCH -L cscratch1

##Directives for the "DataWarp" or "BurstBuffer"
#BB create_persistent name=sodasi_3 capacity=2500GB access_mode=striped type=scratch
