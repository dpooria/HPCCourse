#!/bin/sh
#BSUB -J pi
#BSUB -o pi_%J.out
#BSUB -q hpc
#BSUB -n 24 # specify number of cores
#BSUB -W 10:00 # specify maximum wall clock time
#BSUB -u s000000@student.dtu.dk
#BSUB -R "span[hosts=1]" # reserve all cores on one node
##BSUB -R "span[ptile=1]" # reserve one core per node
#BSUB -R "rusage[mem=16GB]"
#BSUB -B # send notification at start
#BSUB -N # send notification at end


module add studio
module add mpi/3.1.3-oracle-12u6

f90 -free -O3 pi_sequential.f90 -o pi_sequential
echo "" > pi_sequential_res.dat
./pi_sequential

mpif90 -free -O3 pi_mpi_fixedIterations.f90 -o pi_mpi_fixedIterations
mpif90 -free -O3 pi_mpi.f90 -o pi_mpi

echo "" > pi_mpi_res.dat
echo "" > pi_mpi_fixedIterations_res.dat

for n in 1 2 4 8 12 16 24
do
    mpirun -n ${n} ./pi_mpi
done

for n in 1 2 4 8 12 16 24
do
    mpirun -n ${n} ./pi_mpi_fixedIterations
done

