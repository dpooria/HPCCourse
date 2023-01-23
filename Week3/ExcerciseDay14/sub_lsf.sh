#!/bin/bash

# https://www.hpc.dtu.dk/?page_id=1416
#BSUB -q hpc
# use the same CPU model as the hpcintro queue
#BSUB -R "select[model == XeonE5_2650v4]"
#BSUB -J My_job
# ask for 24 cores (a full node)
#BSUB -n 8
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

#Sequential

cat > sample_input.txt <<- END

Nt = 1000
Nx = 4096
Ny = 4096
D = 1.0
Dt = 1E-8
vb = .FALSE.

END

make -f Makefile.sequential new
./Diffusion sequential_details.txt


#MPI
# make new

# for num_cores in 2 4 8
# do
# cat > sample_input.txt <<- END

# Nt = 1000
# Nx = 4096
# Ny = 4096
# D = 1.0
# Dt = 1E-8
# O = "mpi_n${num_cores}.txt"

# END

# mpirun -np ${num_cores} ./Diffusion

# done
