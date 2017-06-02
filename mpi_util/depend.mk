# This file is generated automatically. DO NOT EDIT!

fll_mpi_cp_all.o :  \
	../data_util/fll_mv.o \
	../data_util/fll_type.o \
	../data_util/fll_mk.o \
	../data_util/fll_out.o

fll_mpi_sum.o :  \
	../data_util/fll_mods.o

fll_mpi_write_nm.o :  \
	../data_util/fll_mods.o \
	fll_mpi_write.o

fll_mpi_mods.o :  \
	fll_mpi_write_snm.o \
	fll_mpi_cp.o \
	fll_mpi_proc_struct.o \
	fll_mpi_write_nm.o \
	fll_mpi_write.o \
	fll_mpi_sum.o \
	fll_mpi_mv.o \
	fll_mpi_read.o \
	fll_mpi_cp_all.o

fll_mpi_write_snm.o :  \
	../data_util/fll_mods.o \
	fll_mpi_cp.o

fll_mpi_proc_struct.o :  \
	../data_util/fll_mods.o

fll_mpi_read.o :  \
	../data_util/fll_mods.o \
	fll_mpi_cp_all.o

fll_mpi_mv.o :  \
	fll_mpi_cp.o \
	../data_util/fll_type.o \
	../data_util/fll_out.o \
	../data_util/fll_rm.o

fll_mpi_write.o :  \
	../data_util/fll_mods.o \
	fll_mpi_sum.o

fll_mpi_cp.o :  \
	../data_util/fll_mv.o \
	../data_util/fll_type.o \
	../data_util/fll_mk.o \
	../data_util/fll_out.o
