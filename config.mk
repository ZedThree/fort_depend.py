
SHELL              = /bin/bash
#
# Compilers and flags
#
FC                 = gfortran
FCMODINCFLAG       = -I
FFLAGS             = -g -ffpe-trap=invalid,zero,overflow -fdefault-real-8 -fdefault-double-8 -fconvert=big-endian -frecord-marker=4 -fbounds-check -Wall -Wextra -fopenmp -x f95-cpp-input
FFFLAGS            = -g -ffpe-trap=invalid,zero,overflow -fdefault-real-8 -fdefault-double-8 -ffixed-form -fconvert=big-endian -frecord-marker=4 -fbounds-check -Wall -Wextra -fopenmp -x f95-cpp-input
UPPER_MODFILE_NAME = 
LDFLAGS            = -g -ffpe-trap=invalid,zero,overflow -fdefault-real-8 -fdefault-double-8 -fconvert=big-endian -frecord-marker=4 -fbounds-check -Wall -Wextra -fopenmp
CC                 = gcc -fopenmp
CFLAGS             = -g -fopenmp
C_LDFLAGS          = -g -fopenmp
MPI_FC             = mpif90
MPI_FFLAGS         = -g -ffpe-trap=invalid,zero,overflow -fdefault-real-8 -fdefault-double-8 -fconvert=big-endian -frecord-marker=4 -fbounds-check -Wall -Wextra -fopenmp -x f95-cpp-input
MPI_LDFLAGS        = -g -ffpe-trap=invalid,zero,overflow -fdefault-real-8 -fdefault-double-8 -fconvert=big-endian -frecord-marker=4 -fbounds-check -Wall -Wextra -fopenmp
#
# Global settings.
#
# prefix is the base directory where executables will be installed.
#
prefix  = /home/jka/OSS_CFD/exec/data_util/exec/x86_64
#
# MACHINE identifies the host machine type
#
MACHINE = x86_64
#
# Install command (or cp -p if install is not found)
#
INSTALL = install -c
#
# Optional external libraries
# An empty definition means that this library is not available.
#
LIBM3L    = -L/home/jka/Cprograms/Sources/libm3l/Source/ -lm3l -Wl,-rpath=/home/jka/Cprograms/Sources/libm3l/Source/ -lpthread
LSIPDX    = -L/home/jka/Cprograms/Sources/lsipdx/Source -llsipdx -Wl,-rpath=/home/jka/Cprograms/Sources/lsipdx/Source
LIBM3LPATH    = /home/jka/Cprograms/Sources/libm3l/Source/
LSIPDXPATH    = /home/jka/Cprograms/Sources/lsipdx/Source

MAKEDEPEND=/home/jka/OSS_CFD/trunk/fort_depend.py

