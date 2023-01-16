#!/bin/bash

cat > .tmp <<-END
    set term jpeg;
    set key outside;
    set key below;
    set xlabel 'log_{10}(N*M)';
    set ylabel 'Execution Time (sec)';
    set grid;
    p 'mxv_serial_performance.txt' u 2:3 pt 7 ps 2 title 'sequential', \\
     'mxv_autopar_performance.txt' u 2:3  pt 7 ps 2  title 'automatic parallelization N=8',\\
     'mxv_omp2_performance.txt' u 2:3 pt 7 ps 2 title 'openmp N=2',\\
     'mxv_omp4_performance.txt' u 2:3 pt 7 ps 2 title 'openmp N=4',\\
     'mxv_omp8_performance.txt' u 2:3 pt 7 ps 2 title 'openmp N=8';
    repl;
    quit;
END
gnuplot .tmp > ex1b_performance_logNMt.jpeg

cat > .tmp <<-END
    set term jpeg;
    set key outside;
    set key below;
    set xlabel 'log_{10}(N*M)';
    set ylabel '1 / (Execution time)';
    set grid;
    p 'mxv_serial_performance.txt' u 2:4 pt 7 ps 2 title 'sequential', \\
     'mxv_autopar_performance.txt' u 2:4  pt 7 ps 2  title 'automatic parallelization N=8',\\
     'mxv_omp2_performance.txt' u 2:4 pt 7 ps 2 title 'openmp N=2',\\
     'mxv_omp4_performance.txt' u 2:4 pt 7 ps 2 title 'openmp N=4',\\
     'mxv_omp8_performance.txt' u 2:4 pt 7 ps 2 title 'openmp N=8';
    repl;
    quit;
END
gnuplot .tmp > ex1b_performance_logNMti.jpeg

cat > .tmp <<-END
    set term jpeg;
    set key outside;
    set key below;
    set xlabel 'N*M';
    set ylabel 'Execution time (sec)';
    set grid;
    p 'mxv_serial_performance.txt' u 1:3 pt 7 ps 2 title 'sequential', \\
     'mxv_autopar_performance.txt' u 1:3  pt 7 ps 2  title 'automatic parallelization N=8',\\
     'mxv_omp2_performance.txt' u 1:3 pt 7 ps 2 title 'openmp N=2',\\
     'mxv_omp4_performance.txt' u 1:3 pt 7 ps 2 title 'openmp N=4',\\
     'mxv_omp8_performance.txt' u 1:3 pt 7 ps 2 title 'openmp N=8';
    repl;
    quit;
END
gnuplot .tmp > ex1b_performance_NMt.jpeg

cat > .tmp <<-END
    set term jpeg;
    set key outside;
    set key below;
    set xlabel 'N*M';
    set ylabel '1 / (Execution time)';
    set grid;
    p 'mxv_serial_performance.txt' u 1:4 pt 7 ps 2 title 'sequential', \\
     'mxv_autopar_performance.txt' u 1:4  pt 7 ps 2  title 'automatic parallelization N=8',\\
     'mxv_omp2_performance.txt' u 1:4 pt 7 ps 2 title 'openmp N=2',\\
     'mxv_omp4_performance.txt' u 1:4 pt 7 ps 2 title 'openmp N=4',\\
     'mxv_omp8_performance.txt' u 1:4 pt 7 ps 2 title 'openmp N=8';
    repl;
    quit;
END

gnuplot .tmp > ex1b_performance_NMti.jpeg

rm .tmp
