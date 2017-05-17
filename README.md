# fll is a fortran linked list library with possibility of paralell IO

The library enables operations with linked list.
For more information see attached ppt or pdf file in the project root directory

1. CONFIGURATION
====================================================

Make a new directory.
In this directory execute configure script from FLL project source directory.
For example, if the FLL source is in 

/home/usr/fll_souce/fll-master

and you want to compile and install everyting in 

/home/usr/fll_exec

directory, then do:

mkdir /home/usr/fll_exec

cd /home/usr/fll_exec

/home/usr/fll_souce/master/configure.py -c compiler

where compiler specifies fortran compile options

avalable are:

        gfortran
        gfortran_debug
        x86_64  
        x86_64_debug

x86_64 are settings for Intel fortran compiler



2. COMPILATION:
====================================================

To compile, type

gmake

3. Development:
====================================================


3.1 Adding a new file to existing subdirectory

if adding new file to already existing directory, after adding it type 

gmake depend 

to update the dependencies. If the file depends on files in different subdirectories then specified in FMODIR in the Makefile, add this subdirectory to FMODIR list

3.2 Adding a new subdirectory


When adding new directory to the project, copy a Makefile and src_dir_path.mk from one of already existing directories for example from mpi_util. src_dir_path.mk is not to change. In Makefile the only item which is to be likely edited is
FMODIR array which contains list of directories with files which are used by newly added files.

For example in mpi_util Makfile, the FMODIR array contains ../data_util specifying that fortran source file in mpi_util directory depends on files contained in ../data_util directory and when making dependencies python dependency script will look into this directory

if FMODIR is not specified, the fortran dependency script will search all directories specified in PROJ_ROOT_PATH which is specified in config.mk file in root direcotry of the project. This file is created automatically upon initialization of the project by invoking command

gmake init

in root directory of the fll project




Edit array 


[![Analytics](https://ga-beacon.appspot.com/UA-86532469-1/libm3l/fll)](https://github.com/igrigorik/ga-beacon)
