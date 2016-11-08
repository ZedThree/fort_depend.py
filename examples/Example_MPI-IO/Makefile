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

EXE    = MPI_Test

include src_dir_path.mk
include ../../config.mk

DEP_FILE=project.dep

# Use MPI compilation in this file
FC     =$(MPI_FC)
FFLAGS =$(MPI_FFLAGS) $(FINCFLAGS)


FMODDIRS= ../../data_util ../../mpi_util

FFILES= $(notdir $(wildcard $(srcdir)/*.f90))


OFILES=$(FFILES:%.f90=%.o)
XOFILES=$(wildcard ../../data_util/*.o ../../mpi_util/*.o)

###########################################################################

all: $(EXE).x 

include ../../rules.mk

$(EXE).x: $(OFILES) $(XOFILES)
	$(FC) $(LDFLAGS) -o $@ $^

clean:
	rm -f *.x *.o *.mod $(EXE)

depend: $(FFILES)
	@echo "Making dependencies!"
	cd $(srcdir) && $(MAKEDEPEND) -r $(PROJ_ROOT_PATH) -w -o $(srcdir)/$(DEP_FILE) -f $(FFILES)

install: $(EXE).x $(EXE)
	$(INSTALL) $(EXE) $(bindir)/bin/$(EXE)$(POSTFIX)
	$(INSTALL) $(EXE).x $(bindir)/bin/$(EXE)$(POSTFIX).x

-include $(srcdir)/project.dep
