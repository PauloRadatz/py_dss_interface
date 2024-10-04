#===============================================================================
# UFconfig.mk:  common configuration file for the SuiteSparse
#===============================================================================

# This file contains all configuration settings for all packages authored or
# co-authored by Tim Davis at the University of Florida:
#
# Package Version       Description
# ------- -------       -----------
# AMD	  1.2 or later  approximate minimum degree ordering
# COLAMD  2.4 or later  column approximate minimum degree ordering
# CCOLAMD 1.0 or later  constrained column approximate minimum degree ordering
# CAMD    any		constrained approximate minimum degree ordering
# UMFPACK 4.5 or later	sparse LU factorization, with the BLAS
# CHOLMOD any		sparse Cholesky factorization, update/downdate
# KLU	  0.8 or later  sparse LU factorization, BLAS-free
# BTF	  0.8 or later  permutation to block triangular form
# LDL	  1.2 or later	concise sparse LDL'
# LPDASA  any		linear program solve (dual active set algorithm)
# CXSparse any		extended version of CSparse (int/long, real/complex)
# SuiteSparseQR	any	sparse QR factorization
#
# By design, this file is NOT included in the CSparse / CSparse3 makefiles.
# Those packages are fully stand-alone.
#
# The UFconfig directory and the above packages should all appear in a single
# directory, in order for the Makefile's within each package to find this file.
#
# To enable an option of the form "# OPTION = ...", edit this file and
# delete the "#" in the first column of the option you wish to use.
#
# The use of METIS 4.0.1 is optional.  To exclude METIS, you must compile with
# CHOLMOD_CONFIG set to -DNPARTITION.  See below for details.

#------------------------------------------------------------------------------
# Generic configuration
#------------------------------------------------------------------------------

# Using standard definitions from the make environment, typically:
#
#   CC              cc      C compiler
#   CXX             g++     C++ compiler
#   CFLAGS          [ ]     flags for C and C++ compiler
#   CPPFLAGS        [ ]     flags for C and C++ compiler
#   TARGET_ARCH     [ ]     target architecture
#   FFLAGS          [ ]     flags for Fortran compiler
#   RM              rm -f   delete a file
#   AR              ar      create a static *.a library archive
#   ARFLAGS         rv      flags for ar
#   MAKE            make    make itself (sometimes called gmake)
#
# You can redefine them here, but by default they are used from the
# default make environment.
CC = g++
CFLAGS = -fpermissive

# C and C++ compiler flags.  The first three are standard for *.c and *.cpp
CF = $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -O3 -fexceptions -fPIC

# ranlib, and ar, for generating libraries.  If you don't need ranlib,
# just change it to RANLAB = echo
RANLIB = ranlib
ARCHIVE = $(AR) $(ARFLAGS)

# copy, delete, and rename a file
CP = cp -f
MV = mv -f
# RM = rm -f

# Fortran compiler (not required for 'make' or 'make library')
F77 = gfortran
F77FLAGS = $(FFLAGS) -O
F77LIB =

# C and Fortran libraries
LIB = -lm

# For "make install"
INSTALL_LIB = /usr/local/lib
INSTALL_INCLUDE = /usr/local/include

# For compiling MATLAB mexFunctions (MATLAB 7.5 or later)
MEX = mex -O -largeArrayDims -lmwlapack -lmwblas

# For compiling MATLAB mexFunctions (MATLAB 7.3 and 7.4)
# MEX = mex -O -largeArrayDims -lmwlapack

# For MATLAB 7.2 or earlier, you must use one of these options:
# MEX = mex -O -lmwlapack
# MEX = mex -O

# Which version of MAKE you are using (default is "make")
# MAKE = make
# MAKE = gmake

#------------------------------------------------------------------------------
# BLAS and LAPACK configuration:
#------------------------------------------------------------------------------

# UMFPACK and CHOLMOD both require the BLAS.  CHOLMOD also requires LAPACK.
# See Kazushige Goto's BLAS at http://www.cs.utexas.edu/users/flame/goto/ or
# http://www.tacc.utexas.edu/~kgoto/ for the best BLAS to use with CHOLMOD.
# LAPACK is at http://www.netlib.org/lapack/ .  You can use the standard
# Fortran LAPACK along with Goto's BLAS to obtain very good performance.
# CHOLMOD gets a peak numeric factorization rate of 3.6 Gflops on a 3.2 GHz
# Pentium 4 (512K cache, 4GB main memory) with the Goto BLAS, and 6 Gflops
# on a 2.5Ghz dual-core AMD Opteron.

# These settings will probably not work, since there is no fixed convention for
# naming the BLAS and LAPACK library (*.a or *.so) files.

# This is probably slow ... it might connect to the Standard Reference BLAS:
BLAS = -lblas -lgfortran
LAPACK = -llapack

# NOTE: this next option for the "Goto BLAS" has nothing to do with a "goto"
# statement.  Rather, the Goto BLAS is written by Dr. Kazushige Goto.
# Using the Goto BLAS:
# BLAS = -lgoto -lgfortran -lgfortranbegin

# Using non-optimized versions:
# BLAS = -lblas_plain -lgfortran -lgfortranbegin
# LAPACK = -llapack_plain

# BLAS = -lblas_plain -lgfortran -lgfortranbegin
# LAPACK = -llapack

# The BLAS might not contain xerbla, an error-handling routine for LAPACK and
# the BLAS.  Also, the standard xerbla requires the Fortran I/O library, and
# stops the application program if an error occurs.  A C version of xerbla
# distributed with this software (UFconfig/xerbla/libcerbla.a) includes a
# Fortran-callable xerbla routine that prints nothing and does not stop the
# application program.  This is optional.
# XERBLA = ../../UFconfig/xerbla/libcerbla.a 

# If you wish to use the XERBLA in LAPACK and/or the BLAS instead,
# use this option:
XERBLA = 

# If you wish to use the Fortran UFconfig/xerbla/xerbla.f instead, use this:
# XERBLA = ../../UFconfig/xerbla/libxerbla.a 

#------------------------------------------------------------------------------
# METIS, optionally used by CHOLMOD
#------------------------------------------------------------------------------

# If you do not have METIS, or do not wish to use it in CHOLMOD, you must
# compile CHOLMOD with the -DNPARTITION flag.

# The path is relative to where it is used, in CHOLMOD/Lib, CHOLMOD/MATLAB, etc.
# You may wish to use an absolute path.  METIS is optional.  Compile
# CHOLMOD with -DNPARTITION if you do not wish to use METIS.
METIS_PATH = ../../metis-4.0
METIS = ../../metis-4.0/libmetis.a

#------------------------------------------------------------------------------
# UMFPACK configuration:
#------------------------------------------------------------------------------

# Configuration flags for UMFPACK.  See UMFPACK/Source/umf_config.h for details.
#
# -DNBLAS	do not use the BLAS.  UMFPACK will be very slow.
# -D'LONGBLAS=long' or -DLONGBLAS='long long' defines the integers used by
#  		LAPACK and the BLAS (defaults to 'int')
# -DNSUNPERF	do not use the Sun Perf. Library (default is use it on Solaris)
# -DNPOSIX	do not use POSIX routines sysconf and times.
# -DGETRUSAGE	use getrusage
# -DNO_TIMER	do not use any timing routines
# -DNRECIPROCAL	do not multiply by the reciprocal
# -DNO_DIVIDE_BY_ZERO	do not divide by zero
# -DNCHOLMOD    do not use CHOLMOD as a ordering method.  If -DNCHOLMOD is
#               included in UMFPACK_CONFIG, then UMFPACK  does not rely on
#               CHOLMOD, CAMD, CCOLAMD, COLAMD, and METIS.

UMFPACK_CONFIG =

# uncomment this line to compile UMFPACK without CHOLMOD:
# UMFPACK_CONFIG = -DNCHOLMOD

#------------------------------------------------------------------------------
# CHOLMOD configuration
#------------------------------------------------------------------------------

# CHOLMOD Library Modules, which appear in libcholmod.a:
# Core		requires: none
# Check		requires: Core
# Cholesky	requires: Core, AMD, COLAMD.  optional: Partition, Supernodal
# MatrixOps	requires: Core
# Modify	requires: Core
# Partition	requires: Core, CCOLAMD, METIS.  optional: Cholesky
# Supernodal	requires: Core, BLAS, LAPACK
#
# CHOLMOD test/demo Modules (all are GNU GPL, do not appear in libcholmod.a):
# Tcov		requires: Core, Check, Cholesky, MatrixOps, Modify, Supernodal
#		optional: Partition
# Valgrind	same as Tcov
# Demo		requires: Core, Check, Cholesky, MatrixOps, Supernodal
#		optional: Partition
#
# Configuration flags:
# -DNCHECK	    do not include the Check module.	   License GNU LGPL
# -DNCHOLESKY	    do not include the Cholesky module.	   License GNU LGPL
# -DNPARTITION	    do not include the Partition module.   License GNU LGPL
#		    also do not include METIS.
# -DNGPL	    do not include any GNU GPL Modules in the CHOLMOD library:
# -DNMATRIXOPS	    do not include the MatrixOps module.   License GNU GPL
# -DNMODIFY	    do not include the Modify module.      License GNU GPL
# -DNSUPERNODAL     do not include the Supernodal module.  License GNU GPL
#
# -DNPRINT	    do not print anything.
# -D'LONGBLAS=long' or -DLONGBLAS='long long' defines the integers used by
#  		    	LAPACK and the BLAS (defaults to 'int')
# -DNSUNPERF	    for Solaris only.  If defined, do not use the Sun
#			Performance Library

CHOLMOD_CONFIG =

# uncomment this line to compile CHOLMOD without METIS:
# CHOLMOD_CONFIG = -DNPARTITION

#------------------------------------------------------------------------------
# SuiteSparseQR configuration:
#------------------------------------------------------------------------------

# The SuiteSparseQR library can be compiled with the following options:
#
# -DNPARTITION      do not include the CHOLMOD partition module
# -DNEXPERT         do not include the functions in SuiteSparseQR_expert.cpp
# -DTIMING          enable timing and flop counts
# -DHAVE_TBB        enable the use of Intel's Threading Building Blocks (TBB)

# default, without timing, without TBB:
SPQR_CONFIG =
# with timing and TBB:
# SPQR_CONFIG = -DTIMING -DHAVE_TBB
# with timing
# SPQR_CONFIG = -DTIMING

# This is needed for IBM AIX: (but not for and C codes, just C++)
# SPQR_CONFIG = -DBLAS_NO_UNDERSCORE

# with TBB, you must select this:
# TBB = -ltbb
# without TBB:
TBB =

# with timing, you must include the timing library:
# RTLIB = -lrt
# without timing
RTLIB =

#------------------------------------------------------------------------------
# Linux
#------------------------------------------------------------------------------

# Using default compilers:
# CC = gcc
# CFLAGS = -O3 -fexceptions

# alternatives:
# CFLAGS = -g -fexceptions \
   	-Wall -W -Wshadow -Wmissing-prototypes -Wstrict-prototypes \
    	-Wredundant-decls -Wnested-externs -Wdisabled-optimization -ansi \
        -funit-at-a-time
# CFLAGS = -O3 -fexceptions \
   	-Wall -W -Werror -Wshadow -Wmissing-prototypes -Wstrict-prototypes \
    	-Wredundant-decls -Wnested-externs -Wdisabled-optimization -ansi
# CFLAGS = -O3 -fexceptions -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE
# CFLAGS = -O3
# CFLAGS = -O3 -g -fexceptions
# CFLAGS = -g -fexceptions \
   	-Wall -W -Wshadow \
    	-Wredundant-decls -Wdisabled-optimization -ansi

# consider:
# -fforce-addr -fmove-all-movables -freduce-all-givs -ftsp-ordering
# -frename-registers -ffast-math -funroll-loops

# Using the Goto BLAS:
# BLAS = -lgoto -lfrtbegin -lg2c $(XERBLA) -lpthread

# Using Intel's icc and ifort compilers:
#   (does not work for mexFunctions unless you add a mexopts.sh file)
# F77 = ifort
# CC = icc
# CFLAGS = -O3 -xN -vec_report=0
# CFLAGS = -g
# old (broken): CFLAGS = -ansi -O3 -ip -tpp7 -xW -vec_report0

# 64bit:
# F77FLAGS = -O -m64
# CFLAGS = -O3 -fexceptions -m64
# BLAS = -lgoto64 -lfrtbegin -lg2c -lpthread $(XERBLA)
# LAPACK = -llapack64


# SUSE Linux 10.1, AMD Opteron, with GOTO Blas
# F77 = gfortran
# BLAS = -lgoto_opteron64 -lgfortran

# SUSE Linux 10.1, Intel Pentium, with GOTO Blas
# F77 = gfortran
# BLAS = -lgoto -lgfortran

#------------------------------------------------------------------------------
# Mac
#------------------------------------------------------------------------------

# As recommended by macports, http://suitesparse.darwinports.com/
# I've tested them myself on Mac OSX 10.6.1 (Snow Leopard), on my MacBook Air.
# F77 = gfortran
# CFLAGS = -O3 -fno-common -no-cpp-precomp -fexceptions
# BLAS = -framework Accelerate
# LAPACK = -framework Accelerate

# Using netlib.org LAPACK and BLAS compiled by gfortran, with and without
# optimzation:
# BLAS = -lblas_plain -lgfortran
# LAPACK = -llapack_plain
# BLAS = -lblas_optimized -lgfortran
# LAPACK = -llapack_optimized

#------------------------------------------------------------------------------
# Solaris
#------------------------------------------------------------------------------

# 32-bit
# CFLAGS = -KPIC -dalign -xc99=%none -Xc -xlibmieee -xO5 -xlibmil -m32

# 64-bit
# CFLAGS = -fast -KPIC -xc99=%none -xlibmieee -xlibmil -m64 -Xc

# FFLAGS = -fast -KPIC -dalign -xlibmil -m64

# The Sun Performance Library includes both LAPACK and the BLAS:
# BLAS = -xlic_lib=sunperf
# LAPACK =


#------------------------------------------------------------------------------
# Compaq Alpha
#------------------------------------------------------------------------------

# 64-bit mode only
# CFLAGS = -O2 -std1
# BLAS = -ldxml
# LAPACK =

#------------------------------------------------------------------------------
# Macintosh
#------------------------------------------------------------------------------

# CC = gcc
# CFLAGS = -O3 -fno-common -no-cpp-precomp -fexceptions
# LIB = -lstdc++
# BLAS = -framework Accelerate
# LAPACK = -framework Accelerate

#------------------------------------------------------------------------------
# IBM RS 6000
#------------------------------------------------------------------------------

# BLAS = -lessl
# LAPACK =

# 32-bit mode:
# CFLAGS   = -O4 -qipa -qmaxmem=16384 -qproto
# F77FLAGS = -O4 -qipa -qmaxmem=16384

# 64-bit mode:
# CFLAGS   = -O4 -qipa -qmaxmem=16384 -q64 -qproto
# F77FLAGS = -O4 -qipa -qmaxmem=16384 -q64
# AR = ar -X64

#------------------------------------------------------------------------------
# SGI IRIX
#------------------------------------------------------------------------------

# BLAS = -lscsl
# LAPACK =

# 32-bit mode
# CFLAGS = -O

# 64-bit mode (32 bit int's and 64-bit long's):
# CFLAGS = -64
# F77FLAGS = -64

# SGI doesn't have ranlib
# RANLIB = echo

#------------------------------------------------------------------------------
# AMD Opteron (64 bit)
#------------------------------------------------------------------------------

# BLAS = -lgoto_opteron64 -lg2c
# LAPACK = -llapack_opteron64

# SUSE Linux 10.1, AMD Opteron
# F77 = gfortran
# BLAS = -lgoto_opteron64 -lgfortran
# LAPACK = -llapack_opteron64

#------------------------------------------------------------------------------
# remove object files and profile output
#------------------------------------------------------------------------------

CLEAN = *.o *.obj *.ln *.bb *.bbg *.da *.tcov *.gcov gmon.out *.bak *.d *.gcda *.gcno
