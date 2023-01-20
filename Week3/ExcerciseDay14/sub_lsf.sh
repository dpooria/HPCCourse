#!/bin/bash

# https://www.hpc.dtu.dk/?page_id=1416
#BSUB -q hpc
# use the same CPU model as the hpcintro queue
#BSUB -R "select[model == XeonE5_2650v4]"
#BSUB -J My_job
# ask for 24 cores (a full node)
#BSUB -n 2
#BSUB -R "span[hosts=1]"
##BSUB -R "span[ptile=1]"
#BSUB -R "rusage[mem=2GB]"
#BSUB -W 180
#BSUB -o Output_%J.out
#BSUB -e Error_%J.err

# module load gcc/11.3.0-binutils-2.38
#module add mpi/4.1.4-gcc-11.3.0-binutils-2.38
module add studio
module add mpi/3.1.3-oracle-12u6
lscpu

#Jacobi
# make one full data to ensure the results are correct
cat > sample_input.txt <<- END

Nt = 100
Nx = 4097
Ny = 4097
D = 1.0
Dt = 1E-8
O = "res.txt"

END

echo "" > "res.txt"

make new

mpirun -np 2 ./Diffusion "Diffusion_res_np2"
