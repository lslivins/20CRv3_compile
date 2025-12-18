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
#SBATCH -J stagein_HDF5

#### where does standard error and standard out go? 
#SBATCH -e stagein_HDF5.err
#SBATCH -o stagein_HDF5.out

##### is this for Haswell or KNL
#SBATCH -C knl,cache,quad

#### knl has 68 cpus, only using up to 65 MPI ranks
## so, give 2 for the OS
#SBATCH -S 2

#### make the job depend on cscratch1 being available
#SBATCH -L cscratch1

#### Specify which persistent reservation you will access. Remember to use the correct reservation name!
#DW persistentdw name=V46
#DW stage_in source=/global/cscratch1/sd/compo/V46_stage_in destination=$DW_PERSISTENT_STRIPED_V46 type=directory
#DW stage_out source=$DW_PERSISTENT_STRIPED_V46/outfiles destination=/global/cscratch1/sd/compo/V46_stage_out/outfiles type=directory


# set the observation directory to this persistent reservation name


setenv obsdirh5 $DW_PERSISTENT_STRIPED_V46

mkdir -p $obsdirh5/outfiles

cd $obsdirh5

foreach tgfile (*.tar.gz)

tar -xvzf $tgfile 

end

# go to dir where job was submitted


#cd $PBS_O_WORKDIR
cd $SLURM_SUBMIT_DIR

date; csh ./test_h5_reader_bb.csh ; echo "out" ; date

#### print the mount point of your BB allocation on the compute nodes
echo "*** The path to my BB allocation is:"
echo $DW_PERSISTENT_STRIPED_V46
echo "*** what is on the reservation "
ls -l $DW_PERSISTENT_STRIPED_V46
echo " " 
echo "*** what is in the outfile directory "
ls -l $obsdirh5/outfiles

exit
