
SHELL       = /bin/sh
F77         = gfortran

CC          = cc
DEBUG       = -C
DEBUG       = 
OPT         = -O3
# OPT         = 
FFLAGS      = $(OPT) -ffree-form $(DEBUG)
LD          = $(F77)
CPP         = /lib/cpp

AUTOPARFLAGS = -ftree-parallelize-loops=8 -ffast-math
OPENMPFLAGS	 = -fopenmp -ffast-math

TARGET = ex1_pi_plain.o\
		ex1_pi_autopar.o\
		ex1_pi_openmp_custom_reduction.o\
		ex1_pi_openmp_reduction_clause.o

all: $(TARGET)

ex1_pi_plain.o : ex1_pi_plain.f90
	$(F77) $(FFLAGS) -o ex1_pi_plain.o ex1_pi_plain.f90

ex1_pi_autopar.o : ex1_pi_plain.f90
	$(F77) $(FFLAGS) $(AUTOPARFLAGS) -o ex1_pi_autopar.o ex1_pi_plain.f90

ex1_pi_openmp_custom_reduction.o : ex1_pi_openmp_custom_reduction.f90
	$(F77) $(FFLAGS) $(OPENMPFLAGS) -o ex1_pi_openmp_custom_reduction.o ex1_pi_openmp_custom_reduction.f90
ex1_pi_openmp_reduction_clause.o : ex1_pi_openmp_reduction_clause.f90
	$(F77) $(FFLAGS) $(OPENMPFLAGS) -o ex1_pi_openmp_reduction_clause.o ex1_pi_openmp_reduction_clause.f90


run: 
	./exec.sh

new: cleanall $(TARGET)

cleanall:
	rm *.o -f \
	rn *.txt -f

clean: 
	rm *.o -f

plot:
	./ex1_plot_results.sh


