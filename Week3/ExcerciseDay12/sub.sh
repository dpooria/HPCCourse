#!/bin/sh
#BSUB -J job-name
#BSUB -q hpc
#BSUB -n 2 # specify number of cores
#BSUB -W 10:00 # specify maximum wall clock time
#BSUB -u s000000@student.dtu.dk
##BSUB -R "span[hosts=1]" # reserve all cores on one node
#BSUB -R "span[ptile=1]" # reserve one core per node
#BSUB -R "rusage[mem=16GB]"
#BSUB -B # send notification at start
#BSUB -N # send notification at end
module add studio
module add mpi/3.1.3-oracle-12u6
mpirun -np ${LSB_DJOB_NUMPROC} ./a.out
