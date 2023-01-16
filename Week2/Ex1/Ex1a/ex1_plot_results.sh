#!/bin/bash

cat > .tmp <<-END
    set term jpeg;
    set key outside;
    set key below;
    set xlabel 'log_{10}(N)';
    set ylabel '1 / (Execution time)';
    set grid;
    p 'ex1_pi_plain.o_output.txt' u 1:4 pt 7 ps 2 title 'sequential', \\
     'ex1_pi_autopar.o_output.txt' u 1:4  pt 7 ps 2  title 'automatic parallelization N=8',\\
     'ex1_pi_openmp_custom_reduction.o_output.txt' u 1:4 pt 7 ps 2 title 'OpenMP custom N=8',\\
     'ex1_pi_openmp_reduction_clause.o_output.txt' u 1:4 pt 7 ps 2 title 'OpenMP reduction N=8';
    repl;
    quit;
END

gnuplot .tmp > ex1_performance_comparison.jpeg

rm .tmp