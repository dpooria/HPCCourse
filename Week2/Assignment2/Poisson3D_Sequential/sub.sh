#!/bin/bash

# https://www.hpc.dtu.dk/?page_id=1416
#BSUB -q hpc
# use the same CPU model as the hpcintro queue
#BSUB -R "select[model == XeonE5_2650v4]"
#BSUB -J My_job
# ask for 24 cores (a full node)
#BSUB -n 1
#BSUB -R "span[hosts=1]"
#BSUB -R "rusage[mem=2GB]"
#BSUB -W 180
#BSUB -o Output_%J.out
#BSUB -e Error_%J.err 

# add your module, if needed, and uncomment
#module load ...

# If you want to see the HW specs, uncomment the next line


# Set these before running (and uncomment)
# export OMP_PLACES=
# export OMP_PROC_BIND=

# Optional, it will print the OMP environment settings in the error file
# export OMP_DISPLAY_ENV=true

# loop over different number of threads (adjust list if needed)

module load gcc/10.3.0-binutils-2.36.1
lscpu
make new

itermax=10000

#Jacobi
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

./poisson3d fulldata_jacobi.dat

for N in 24 48 64 100 128 164 200
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

./poisson3d "jacobi_N${N}_full_iteration.dat"
done

for N in 24 48 64 100 128 164 200
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

./poisson3d "jacobi_N${N}.dat"
done

#Gauss-Seidel

# make one full data to ensure the results are correct
cat > input.dat <<- END

&INPUT
  N = 100
  itermax = ${itermax}
  T0 = 10.
  tolerance = 1e-5
  output = 1
  algorithm = 2
  full = 1
/

END

./poisson3d fulldata_gaussseidel.dat

for N in 24 48 64 100 128 164 200
do
cat > input.dat <<- END

&INPUT
   N = $N
   itermax = ${itermax}
   T0 = 10.
   tolerance = 0.0
   output = 1
   algorithm = 2
   full = 0
/

END

./poisson3d "gaussseidel_N${N}_full_iteration.dat"

done

for N in 24 48 64 100 128 164 200
do
cat > input.dat <<- END

&INPUT
   N = $N
   itermax = ${itermax}
   T0 = 10.
   tolerance = 1e-5
   output = 1
   algorithm = 2
   full = 0
/

END

./poisson3d "gaussseidel_N${N}.dat"

done


