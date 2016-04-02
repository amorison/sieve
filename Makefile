.PHONY: all clean

FC=gfortran
FFLAGS=-O3

all: sieve

sieve: sieve.o
	$(FC) $(FFLAGS) -o $@ $<

sieve.o: sieve.f90
	$(FC) $(FFLAGS) -o $@ -c $<

clean:
	$(RM) sieve.o sieve
