#!/bin/bash

# https://www.hpc.dtu.dk/?page_id=1416
#BSUB -q hpc
# use the same CPU model as the hpcintro queue
#BSUB -R "select[model == XeonE5_2650v4]"
#BSUB -J My_job
# ask for 24 cores (a full node)
#BSUB -n 24
#BSUB -R "span[hosts=1]"
#BSUB -R "rusage[mem=2GB]"
#BSUB -W 180
#BSUB -o Output_%J.out
#BSUB -e Error_%J.err

# Set these before running (and uncomment)
# export OMP_PLACES=

export OMP_DISPLAY_ENV=verbose
export OMP_PROC_BIND=true

module load gcc/10.3.0-binutils-2.36.1
lscpu
make new

itermax=10000

# make one full data to ensure the results are correct
cat > input.dat <<- END

&INPUT
  N = 100
  itermax = ${itermax}
  T0 = 10.
  tolerance = 1e-5
  output = 1
  algorithm = 1
  full = 1
/

END

OMP_NUM_THREADS=16 ./poisson3d fulldata_jacobi.dat


# loop over different number of threads (adjust list if needed)
for thr in 1 2 4 8 12 16 24
do
   export OMP_NUM_THREADS=$thr
    
   for N in 24 64 100 164
   do
cat > input.dat <<- END

&INPUT
  N = $N
  itermax = ${itermax}
  T0 = 10.
  tolerance = 0.0
  output = 1
  algorithm = 1
  full = 0
/

END
        
       ./poisson3d "jacobi_${thr}threads_N${N}_full_iteration.dat"
   done
    
   for N in 24 64 100 164
   do
cat > input.dat <<- END

&INPUT
  N = $N
  itermax = ${itermax}
  T0 = 10.
   tolerance = 1e-5
   output = 1
   algorithm = 1
   full = 0
/

END
       ./poisson3d "jacobi_${thr}threads_N${N}.dat"       
   done
done

############### without compiler optimizations
make -f Makefile.unoptimized new

itermax=10000
# loop over different number of threads (adjust list if needed)

for thr in 1 2 4 8 12 16 24
do
    export OMP_NUM_THREADS=$thr
    
    for N in 24 64 100 164
    do
cat > input.dat <<- END

&INPUT
   N = $N
   itermax = ${itermax}
   T0 = 10.
   tolerance = 0.0
   output = 1
   algorithm = 1
   full = 0
/

END
        
        ./poisson3d "jacobi_${thr}threads_N${N}_full_iteration_unoptimized.dat"
    done
    
    for N in 24 64 100 164
    do
cat > input.dat <<- END

&INPUT
   N = $N
   itermax = ${itermax}
   T0 = 10.
   tolerance = 1e-5
   output = 1
   algorithm = 1
   full = 0
/

END
        ./poisson3d "jacobi_${thr}threads_N${N}_unoptimized.dat"       
    done
done

