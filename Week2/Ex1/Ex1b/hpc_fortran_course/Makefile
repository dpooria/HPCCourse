
SHELL       = /bin/sh
F77         = gfortran

CC          = cc
DEBUG       = -C
DEBUG       = -Wall
OPT         = -O3 -ffast-math
# OPT         = 
FFLAGS      = $(OPT) -ffree-form $(DEBUG)
LD          = $(F77)
CPP         = /lib/cpp

AUTOPARFLAGS = -ftree-parallelize-loops=8
OPENMPFLAGS	 = -fopenmp

TARGET = ex1b_mxv_serial.o\
		ex1b_mxv_autopar.o\
		ex1b_mxv_openmp.o
# ex1b_mxv_openmp_reduction_clause.o

all: $(TARGET)

ex1b_mxv_serial.o : ex1b_mxv.f90
	$(F77) $(FFLAGS) -o ex1b_mxv_serial.o ex1b_mxv.f90

ex1b_mxv_autopar.o : ex1b_mxv.f90
	$(F77) $(FFLAGS) $(AUTOPARFLAGS) -o ex1b_mxv_autopar.o ex1b_mxv.f90

ex1b_mxv_openmp.o : ex1b_mxv.f90
	$(F77) $(FFLAGS) $(OPENMPFLAGS) -o ex1b_mxv_openmp.o ex1b_mxv.f90

run: 
	./exec.sh

new: cleanall $(TARGET)

cleanall:
	rm *.o -f

plot:
	./ex1b_plot_results.sh