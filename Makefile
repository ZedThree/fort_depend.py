#
#     Copyright (C) 2016  Adam Jirasek
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Lesser General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Lesser General Public License for more details.
# 
#     You should have received a copy of the GNU Lesser General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#     
#     contact: libm3l@gmail.com
# 
#
#
# Description: Makefile 
#
# 
# History:
# Version   Date       Patch number  CLA     Comment
# -------   --------   --------      ---     -------
# 1.1       10/10/16                         Initial implementation
#
#
#
-include src_dir_path.mk
-include config.mk

MPIF = $(shell command -v mpif90)

SUBDIRS= \
data_util\
accessories\

ifneq ($(MPI_FC),)
ifneq ($(MPIF),)
  SUBDIRS+= mpi_util
endif
endif

SUBDIRS+= examples\
miscellaneous\

SUBCLEAN=$(SUBDIRS)

###########################################################################

all: $(SUBDIRS:%=%.all)

	ar rcs fll.a data_util/*.o

	@echo '************************************************'
	@echo '*'
	@echo '* FLL built successfully!'
	@echo '*'
	@echo '************************************************'

include rules.mk


init:
	echo PROJ_ROOT_PATH=$(PWD) > config.mk
	echo MAKEDEPEND=$(PWD)/python_dep/fort_depend.py >> config.mk
	echo MPI_FC=YES >> config.mk
	echo VERBOSE=-vvv >> config.mk

data_util.all: 
test.all: data_util.all

clean: $(SUBCLEAN:%=%.clean)

depend: $(SUBDIRS:%=%.depend)

install: $(SUBDIRS:%=%.all) $(SUBDIRS:%=%.install)
	ar rcs fll.a data_util/*.o
	mv fll.a $(lib_dir)/fll.a

