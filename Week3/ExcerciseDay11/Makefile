#----------------------------------------------------------------------
#                                                                      
#  Makefile : diffuse2     
#  Version  :                                                          
#  Author   : Pooria Dabbaghi                                                  
#  Created  : 
#                                                                      
#----------------------------------------------------------------------
#  Creator: makemake 2.1
#  Rev: 1996/01/11 by Jens H. Walther  
#----------------------------------------------------------------------
SHELL       = /bin/sh
TARGET      = Diffusion
#----------------------------------------------------------------------
#  Compiler settings (Linux)
#----------------------------------------------------------------------
# F77         = gfortran
F77         = f90
CC          = cc
DEBUG       = -C
DEBUG       = 
OPT         = -O3
FFLAGS      = $(OPT) -free $(DEBUG)
# FFLAGS      = $(OPT) -ffree-form $(DEBUG)
CFLAGS      = -O
LD          = $(F77)
LDFLAGS     = 
CPP         = /lib/cpp
DEFINE      = 
LIBS        = 
SRC			= ./src/v1

#----------------------------------------------------------------------
#  Search path for RCS files                                           
#----------------------------------------------------------------------
# VPATH = ./RCS

#----------------------------------------------------------------------
#  Additional suffix rules                                             
#----------------------------------------------------------------------
.SUFFIXES : .inc .inc,v .f90 .f90,v .c,v .f .f,v

.f,v.f :
	 co $*.f

.f90,v.f90 :
	 co $*.f90

.c,v.c :
	 co $*.c

.inc,v.inc :
	 co $*.inc

#----------------------------------------------------------------------
#  Binary directory
#----------------------------------------------------------------------
bindir      = $(HOME)/bin

#----------------------------------------------------------------------
#  Default target
#----------------------------------------------------------------------
all: $(TARGET)

#----------------------------------------------------------------------
#  Object files:                                                       
#  NOTE: you HAVE to sort the objects files such that no file will 
#  depend on files below it ! in this example, the diffuse2.f90 and .o
#  depends on all he module files (i named them m_*.f90), and the m_init
#  depends (USE) the m_diffuse; thus m_diffuse HAS to be compiled 
#  before m_init and before diffuse2
#----------------------------------------------------------------------
OBJS =\
	m_Diffusion_precision.o\
	m_Diffusion.o\
	m_Diffusion_copy.o\
	m_Diffusion_write.o\
	m_Diffusion_alloc.o\
	m_Diffusion_bc.o\
	m_Diffusion_init.o\
	m_Diffusion_diagnostics.o\
	m_Diffusion_parse_params.o\
	Diffusion.o

#----------------------------------------------------------------------
#  Dependencies:                                                       
#  NOTE: add the dependencies here explicitly ! 
#  In that way you are sure diffuse2.f90 will be recompile if any of the
#  modules source files are modified.
#----------------------------------------------------------------------
m_Diffusion_precision.o: $(SRC)/m_Diffusion_precision.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_precision.f90
m_Diffusion_alloc.o: $(SRC)/m_Diffusion_alloc.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_alloc.f90
m_Diffusion.o: $(SRC)/m_Diffusion.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion.f90
m_Diffusion_write.o: $(SRC)/m_Diffusion_write.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_write.f90
m_Diffusion_copy.o: $(SRC)/m_Diffusion_copy.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_copy.f90
m_Diffusion_bc.o: $(SRC)/m_Diffusion_bc.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_bc.f90
m_Diffusion_init.o: $(SRC)/m_Diffusion_init.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_init.f90
m_Diffusion_diagnostics.o: $(SRC)/m_Diffusion_diagnostics.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_diagnostics.f90
m_Diffusion_parse_params.o: $(SRC)/m_Diffusion_parse_params.f90
	$(F77) $(FFLAGS)  -c $(SRC)/m_Diffusion_parse_params.f90
Diffusion.o: $(SRC)/Diffusion.f90 
	$(F77) $(FFLAGS)  -c $(SRC)/Diffusion.f90

#----------------------------------------------------------------------
#  link                                                                
#----------------------------------------------------------------------
$(TARGET): $(OBJS)
	$(LD) -o $(TARGET) $(LDFLAGS) $(OBJS) $(LIBS)

#----------------------------------------------------------------------
#  Install                                                             
#----------------------------------------------------------------------
install: $(TARGET)
	(cp -f $(TARGET) $(bindir))

#----------------------------------------------------------------------
#  Run                                                                 
#----------------------------------------------------------------------
run: ./$(TARGET)
	./$(TARGET)

#----------------------------------------------------------------------
#  Clean                                                               
#----------------------------------------------------------------------
new: cleanall $(TARGET)

cleanall:
	 rm -f __*.f90
	 rm -f __*.f90
	 rm -f $(OBJS)
	 rm -f *.lst
	 rm -f *.mod
	 rm -f src/*/*.mod
	 rm -f *.l
	 rm -f *.L
	 rm -f *.daty
	 rm -rf alloc_data_bkp/*
	 rm -f $(TARGET)
	 rm -f *.o
#  rm -f *.mpeg

clean:
	 rm -f __*.f90
	 rm -f *.lst

plot:
	./Diffusion_plot.sh
