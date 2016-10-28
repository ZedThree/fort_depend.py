
EXE    = fll_test

include src_dir_path.mk
include ../config.mk

DEP_FILE=project.dep

FMODDIRS= ../data_util

FFILES= $(notdir $(wildcard $(srcdir)/*.f90))


OFILES=$(FFILES:%.f90=%.o)
XOFILES=$(wildcard ../data_util/*.o)

###########################################################################

all: $(EXE).x $(EXE)

include ../rules.mk

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
