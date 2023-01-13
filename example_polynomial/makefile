NESTLIBDIR = ..
LIBS = -L$(NESTLIBDIR) -lnest3 $(LAPACKLIB)
 
FC=mpif90
OBJFILES = params.o poly.o like.o nestwrap.o main.o

all: polynomial

%.o: %.f90
	$(FC) $(FFLAGS) -I$(NESTLIBDIR) -c $*.f90
 
polynomial: $(OBJFILES)
	$(FC) -o ../polynomial $(OBJFILES) \
	$(FFLAGS) $(LIBS)

clean:
	rm -f *.o *.mod ../polynomial
