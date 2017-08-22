# This file is generated automatically by fort_depend.py. DO NOT EDIT!

fll_deattach.o :  \
	fll_type.o \
	fll_out.o \
	fll_stich.o

fll_rename.o :  \
	fll_type.o \
	fll_out.o

fll_mk.o :  \
	fll_type.o \
	fll_out.o

fll_mkdir.o :  \
	fll_mk.o \
	fll_type.o

fll_sweep.o :  \
	fll_match_pattern.o \
	fll_type.o \
	fll_out.o

fll_scan_file.o :  \
	fll_type.o \
	fll_read.o \
	fll_out.o

fll_getnbytes.o :  \
	fll_type.o \
	fll_out.o

fll_stich.o :  \
	fll_type.o \
	fll_out.o

fll_funct_prt.o :  \
	fll_type.o \
	fll_out.o

fll_read_record.o :  \
	fll_mv.o \
	fll_read.o \
	fll_out.o \
	fll_rm.o \
	fll_mk.o \
	fll_type.o \
	fll_cat.o \
	fll_locate.o

fll_read.o :  \
	fll_mk.o \
	fll_type.o \
	fll_out.o \
	fll_mv.o \
	fll_funct_prt.o

fll_rm.o :  \
	fll_type.o \
	fll_out.o \
	fll_stich.o

fll_cp.o :  \
	fll_mv.o \
	fll_stich.o \
	fll_out.o \
	fll_duplicate.o \
	fll_rm.o \
	fll_type.o \
	fll_cat.o

fll_cat.o :  \
	fll_type.o \
	fll_out.o

fll_write.o :  \
	fll_type.o \
	fll_out.o

fll_duplicate.o :  \
	fll_mk.o \
	fll_type.o \
	fll_out.o \
	fll_mv.o

fll_getndata.o :  \
	fll_type.o \
	fll_out.o \
	fll_locate.o

fll_match_pattern.o :  \
	fll_type.o \
	fll_out.o

fll_mods.o :  \
	fll_stich.o \
	fll_out.o \
	fll_locate.o \
	fll_mkdir.o \
	fll_getndata.o \
	fll_cp.o \
	fll_getnbytes.o \
	fll_rm.o \
	fll_type.o \
	fll_cat.o \
	fll_write_ffa.o \
	fll_duplicate.o \
	fll_match_pattern.o \
	fll_rename.o \
	fll_scan_file.o \
	fll_read_record.o \
	fll_nnodes.o \
	fll_funct_prt.o \
	fll_deattach.o \
	fll_mv.o \
	fll_read.o \
	fll_sweep.o \
	fll_read_ffa.o \
	fll_write.o \
	fll_mk.o

fll_nnodes.o :  \
	fll_type.o \
	fll_funct_prt.o \
	fll_out.o

fll_mv.o :  \
	fll_type.o \
	fll_rm.o \
	fll_out.o \
	fll_stich.o

fll_type.o : 

fll_out.o :  \
	fll_type.o

fll_locate.o :  \
	fll_type.o \
	fll_funct_prt.o \
	fll_out.o

fll_read_ffa.o :  \
	fll_mk.o \
	fll_type.o \
	fll_out.o \
	fll_mv.o \
	fll_funct_prt.o

fll_write_ffa.o :  \
	fll_type.o \
	fll_out.o
