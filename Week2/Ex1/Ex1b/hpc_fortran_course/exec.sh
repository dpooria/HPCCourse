#!/bin/bash

echo 'Serial Run:'
./ex1b_mxv_serial.o mxv_serial_performance.txt
echo 'Auto parallelization (with 8 threads):'
./ex1b_mxv_autopar.o mxv_autopar_performance.txt
echo 'Openmp (with 2 threads):'
OMP_NUM_THREADS=2 ./ex1b_mxv_openmp.o mxv_omp2_performance.txt
echo 'Openmp (with 4 threads):'
OMP_NUM_THREADS=4 ./ex1b_mxv_openmp.o mxv_omp4_performance.txt
echo 'Openmp (with 8 threads):'
OMP_NUM_THREADS=8 ./ex1b_mxv_openmp.o mxv_omp8_performance.txt

