# KLUSolve Demo Makefile.  

CC = g++

default: all

include ../../UFconfig/UFconfig.mk

LIB = ../Lib/libklusolve.a ../../KLU/Lib/libklu.a ../../BTF/Lib/libbtf.a \
	../../AMD/Lib/libamd.a ../../COLAMD/Lib/libcolamd.a \
	../../CSparse/Lib/libcsparse.a ../../CZSparse/Lib/libczsparse.a

I = -I../../UFconfig -I../../AMD/Include -I../../COLAMD/Include \
	-I../../BTF/Include -I../../CSparse/Include -I../../CZSparse/Include \
	-I../../KLU/Include -I../Include

all: $(LIB) klusolve_demo
	- ./klusolve_demo

../Lib/libklusolve.a:
	( cd ../Lib ; $(MAKE) )

../../KLU/Lib/libklu.a:
	( cd ../../KLU ; $(MAKE) library )

../../BTF/Lib/libbtf.a:
	( cd ../../BTF ; $(MAKE) library )

../../AMD/Lib/libamd.a:
	( cd ../../AMD ; $(MAKE) library )

../../COLAMD/Lib/libcolamd.a:
	( cd ../../COLAMD ; $(MAKE) library )

purge: distclean

distclean: clean
	- $(RM) klusolve_demo

clean:
	- $(RM) $(CLEAN)

klusolve_demo: Makefile $(LIB)
	$(CC) $(CFLAGS) $(I) klusolve_demo.cpp -o klusolve_demo $(LIB) -lm
	- ./klusolve_demo
