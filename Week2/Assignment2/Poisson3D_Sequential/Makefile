target = poisson3d

.SUFFIXES:
.SUFFIXES: .f90 .o .c

CC = gcc
FC = gfortran
FFLAGS = -O3 -ffast-math -funroll-loops -fopenmp

OBJS = poisson3d.o precision.o poisson_methods.o poisson_utils.o write_vtk.o

LIBS =

.PHONY: all
all: $(target)

.PHONY: new
new: clean $(target)

.PHONY: clean realclean
clean:
	@/bin/rm -f $(OBJS) *.mod

realclean: clean
	@/bin/rm -f $(target)

# linking: the target depends on the objects
$(target): $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) -o $(target)

.f90.o:
	$(FC) -c $(FFLAGS) $<

.c.o:
	$(CC) -c $<

# dependencies:
poisson3d.o: precision.o poisson_utils.o poisson_methods.o write_vtk.o
poisson_methods.o: precision.o
poisson_utils.o: precision.o


