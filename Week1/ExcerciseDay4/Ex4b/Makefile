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
TARGET      = Ex4b
#----------------------------------------------------------------------
#  Compiler settings (Linux)
#----------------------------------------------------------------------
# F77         = f90
F77         = gfortran

CC          = cc
DEBUG       = -C
DEBUG       = 
OPT         = -O3
# FFLAGS      = $(OPT) -free $(DEBUG)
FFLAGS      = $(OPT) -ffree-form $(DEBUG)
CFLAGS      = -O
LD          = $(F77)
LDFLAGS     = 
CPP         = /lib/cpp
DEFINE      = 
LIBS        = 
SRC			= ./src

#----------------------------------------------------------------------
#  Search path for RCS files                                           
#----------------------------------------------------------------------
VPATH = ./RCS

#----------------------------------------------------------------------
#  Additional suffix rules                                             
#----------------------------------------------------------------------
.SUFFIXES : .inc .inc,v .f,v .c,v f95,v
.f,v.f :
	 co $*.f

.f95,v.f95 :
	 co $*.f95

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
#  depend on files below it ! in this example, the diffuse2.f and .o
#  depends on all he module files (i named them m_*.f), and the m_init
#  depends (USE) the m_diffuse; thus m_diffuse HAS to be compiled 
#  before m_init and before diffuse2
#----------------------------------------------------------------------
OBJS =\
	m_Ex4b_precision.o\
	m_Ex4b.o\
	m_Ex4b_copy.o\
	m_Ex4b_write.o\
	m_Ex4b_alloc.o\
	m_Ex4b_bc.o\
	m_Ex4b_init.o\
	m_Ex4b_diagnostics.o\
	Ex4b.o

#----------------------------------------------------------------------
#  Dependencies:                                                       
#  NOTE: add the dependencies here explicitly ! 
#  In that way you are sure diffuse2.f will be recompile if any of the
#  modules source files are modified.
#----------------------------------------------------------------------
m_Ex4b_precision.o: $(SRC)/m_Ex4b_precision.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b_precision.f95
m_Ex4b_alloc.o: $(SRC)/m_Ex4b_alloc.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b_alloc.f95
m_Ex4b.o: $(SRC)/m_Ex4b.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b.f95
m_Ex4b_write.o: $(SRC)/m_Ex4b_write.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b_write.f95
m_Ex4b_copy.o: $(SRC)/m_Ex4b_copy.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b_copy.f95
m_Ex4b_bc.o: $(SRC)/m_Ex4b_bc.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b_bc.f95
m_Ex4b_init.o: $(SRC)/m_Ex4b_init.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b_init.f95
m_Ex4b_diagnostics.o: $(SRC)/m_Ex4b_diagnostics.f95
	$(F77) $(FFLAGS)  -c $(SRC)/m_Ex4b_diagnostics.f95
Ex4b.o: $(SRC)/Ex4b.f95 
	$(F77) $(FFLAGS)  -c $(SRC)/Ex4b.f95

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
	 rm -f __*.f
	 rm -f __*.f95
	 rm -f $(OBJS)
	 rm -f *.lst
	 rm -f *.mod
	 rm -f src/*.mod
	 rm -f *.l
	 rm -f *.L
	 rm -f *.daty
	 rm -rf outputs/*
	 rm -rf alloc_data_bkp/*
	 rm -f $(TARGET)
	 rm -f *.o
#  rm -f *.mpeg
clean:
	 rm -f __*.f
	 rm -f *.lst


plot:
	./Ex4b_plot.sh
