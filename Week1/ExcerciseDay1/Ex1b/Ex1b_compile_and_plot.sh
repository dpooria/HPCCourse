#!/bin/bash
gfortran -ffree-form Ex1b.f95 -o Ex1b.out
./Ex1b.out
gnuplot -e "set term post eps color solid; sp 'diff.dat' u 1:2:3 w l; set out 'diff.ps'; repl; quit" > gnuplot.out
