#!/bin/bash

# https://www.hpc.dtu.dk/?page_id=1416
#BSUB -q hpc
# use the same CPU model as the hpcintro queue
#BSUB -R "select[model == XeonE5_2650v4]"
#BSUB -J My_job
# ask for 24 cores (a full node)
#BSUB -n 1
# #BSUB -R "span[hosts=1]"
#BSUB -R "span[ptile=1]"
#BSUB -R "rusage[mem=2GB]"
#BSUB -W 180
#BSUB -o Output_%J.out
#BSUB -e Error_%J.err

module load gcc/11.3.0-binutils-2.38
#module add mpi/4.1.4-gcc-11.3.0-binutils-2.38
lscpu

#Jacobi
# make one full data to ensure the results are correct
cat > sample_input.txt <<- END

Nt = 100
Nx = 4097
Ny = 4097
D = 1.0
Dt = 1E-4
O = "res.txt"

END

# version v1: unmodified from the excercise day5
# version v2: v1 with Pointers insted of allocatable
# version v3: Making improvements on v1 (defining temporary variables and subsitiuting division with multipilication)
# Change optimization level

#single vs double precisions
# for MK in MKS MKD
# do
#     # different versions
#     for v in v1 v2 v3
#     do
# cat > ./src/${v}/m_Diffusion_precision.f90 <<- E

# MODULE m_Diffusion_precision
#         INTEGER, PARAMETER :: MKS = KIND(1.0E0)
#         INTEGER, PARAMETER :: MKD = KIND(1.0D0)
#         INTEGER, PARAMETER :: MK = ${MK}
#         END MODULE m_Diffusion_precision
        
# E
#         # Change the level of
#         for o in O0 O1 O2 O3 O4
#         do
#             make SRC=./src/${v} OPT="-${o}" new
#             ./Diffusion ./outputs/${v}_${o}_only_${MK}_.txt
#             make SRC=./src/${v} OPT="-${o} -ffast-math -funroll-loops" new
#             ./Diffusion ./outputs/${v}_${o}_wtihotheroptimizations_${MK}_.txt
#         done
#     done
# done


