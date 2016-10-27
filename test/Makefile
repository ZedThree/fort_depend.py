############################################################################
#                   Makefile for ffalist
############################################################################
#
# $HeadURL: https://costello.foi.se/svn/edge/branches/aeroelastics/programs/ffalist/Makefile $
# $LastChangedDate: 2007-09-21 14:50:09 +0200 (Fri, 21 Sep 2007) $
# $LastChangedBy: oskeno $
# $LastChangedRevision: 1282 $

EXE    = ll_test

include src_dir_path
include ../config.mk

DEP_FILE=project.dep

FMODDIRS= ../data_util

FFILES= $(notdir $(wildcard $(srcdir)/*.f90))


OFILES=$(FFILES:%.f90=%.o)
XOFILES=$(wildcard $(FMODDIRS)/*.o)

###########################################################################

all: $(EXE).x $(EXE)

include ../rules.mk

$(EXE).x: $(OFILES) $(XOFILES)
	$(FC) $(LDFLAGS) -o $@ $^

clean:
	rm -f *.x *.o *.mod $(EXE)

depend: $(FFILES)
	@echo "Making dependencies!"
	cd $(srcdir) && $(MAKEDEPEND) -r /home/jka/OSS_CFD/trunk/ -w -o $(srcdir)/$(DEP_FILE) -f $(FFILES)

install: $(EXE).x $(EXE)
	$(INSTALL) $(EXE) $(bindir)/bin/$(EXE)$(POSTFIX)
	$(INSTALL) $(EXE).x $(bindir)/bin/$(EXE)$(POSTFIX).x

-include $(srcdir)/project.dep
