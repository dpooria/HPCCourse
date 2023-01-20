#!/bin/bash
##SBATCH --mail-type=ALL
##SBATCH --mail-user=<Your E-mail>  # The default value is the submitting user.
#SBATCH --partition=xeon24
#SBATCH -N 1     # Minimum of 2 nodes
#SBATCH -n 24     # 24 MPI processes per node, 48 tasks in total, appropriate for xeon24 nodes

#SBATCH --time=1-02:00:00
#SBATCH --output=mpi_job_slurm_output.log
#SBATCH --error=mpi_job_slurm_errors.log


module load OpenMPI/4.1.4-GCC-11.3.0
make new


cat > sample_input.txt <<- END

Nt = 100
Nx = 400
Ny = 400
D = 1.0
Dt = 1E-4
O = "res.txt"

END

rm res.txt

mpirun -np 4 ./Diffusion
