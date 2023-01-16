
SHELL       = /bin/sh
TARGET      = Ex3a
F95         = gfortran
CC          = cc
DEBUG       = -C
DEBUG       = 
OPT         = -O3
FFLAGS      = $(OPT) -ffree-form $(DEBUG) -std=f95
CFLAGS      = -O
LD          = $(F95)
LDFLAGS     = 
CPP         = /lib/cpp
DEFINE      = 
LIBS        = 


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
	m_Ex3a_swap.o\
	Ex3a.o\
	
#----------------------------------------------------------------------
#  Dependencies:                                                       
#  NOTE: add the dependencies here explicitly ! 
#  In that way you are sure diffuse2.f will be recompile if any of the
#  modules source files are modified.
#----------------------------------------------------------------------
m_Ex3a_swap.o: m_Ex3a_swap.f95
	$(F95) $(FFLAGS)  -c m_Ex3a_swap.f95

Ex3a.o: Ex3a.f95 
	$(F95) $(FFLAGS)  -c Ex3a.f95

#----------------------------------------------------------------------
#  link                                                                
#----------------------------------------------------------------------
$(TARGET): $(OBJS)
	$(LD) -o $(TARGET) $(LDFLAGS) $(OBJS) $(LIBS)

#----------------------------------------------------------------------
#  Install                                                             
#----------------------------------------------------------------------
# install: $(TARGET)
# 	(cp -f $(TARGET) $(bindir))

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
	 rm -f $(OBJS)
	 rm -f *.lst
	 rm -f *.mod
	 rm -f *.l
	 rm -f *.L

clean:
	 rm -f __*.f
	 rm -f *.lst
