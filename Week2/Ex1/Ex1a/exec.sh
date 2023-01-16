#!/bin/bash
echo 'Serial Run:'
./ex1_pi_plain.o
echo 'Auto parallelization:'
./ex1_pi_autopar.o
echo 'Openmp (with custom reduction):' 
OMP_NUM_THREADS=8 ./ex1_pi_openmp_custom_reduction.o
echo 'Openmp (with reduction clause):' 
OMP_NUM_THREADS=8 ./ex1_pi_openmp_reduction_clause.o

