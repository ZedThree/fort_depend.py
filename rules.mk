
# Mark standard targets as phony (not real file targets).
# This helps avoid problems if there is also a file with the same name.
.PHONY: all clean test depend install cleanall doc chk chk-fix

# Disable implicit rules (some of them may otherwise cause problems).
.SUFFIXES:

# Define macro to handle compilers generating upper-case module filenames
# Note that the variable UPPER_MODFILE_NAME switches this
HANDLE_UPPER_CASE_MOD_NAMES = ! [ "$(UPPER_MODFILE_NAME)" ] || ! [[ $* = *_m ]] || cp `echo $* | tr a-z A-Z`.mod $*.mod

# Rule for building free-form fortran from srcdir directory
%.o %.mod: $(srcdir)/%.f90
	$(FC) -c $(FFLAGS) $(FMODDIRS:%=$(FCMODINCFLAG)%) $<
	-@$(HANDLE_UPPER_CASE_MOD_NAMES)

# Rule for building fixed-form fortran from srcdir directory
%.o %.mod: $(srcdir)/%.f
	$(FC) -c $(FFFLAGS) $(FMODDIRS:%=$(FCMODINCFLAG)%) $<
	-@$(HANDLE_UPPER_CASE_MOD_NAMES)

# Rule for building C from srcdir directory
%.o: $(srcdir)/%.c
	$(CC) -c $(CFLAGS) $(CINCDIRS:%=-I%) $<

# Rule for creating a build directory, if it doesn't already exist.
# Also links the corresponding Makefile from the sourcedirectory if
# it doesn't already exist. Also copies depend.mk from sourcedirectory
# if it exists.
%.mkdir:
	@[ -d $* ] || mkdir $*
	@! [ -h $*/Makefile ] || rm -f $*/Makefile 
	@[ -f $*/Makefile ] || ln -s $(srcdir)/$*/Makefile $*/Makefile
	@rm -f $*/local_config.mk
	@echo srcdir=$(if $(filter .,$(srcdir)),.,$(srcdir)/$*) \
	  > $*/local_config.mk

%: $(srcdir)/%.bash
	sed 's:^#!/bin/bash:#!'"$(SHELL):" $< > $@
	chmod 755 $@

%: $(srcdir)/%.py
	cat $< > $@
	chmod 755 $@

#
# Rules for recursion into subdirectories for standard targets.
# 
%.all: %.mkdir
	@cd $* && $(MAKE) all

%.test: %.mkdir
	@cd $* && $(MAKE) test

%.clean: %.mkdir
	@cd $* && $(MAKE) clean

%.depend: %.mkdir
	@cd $* && $(MAKE) depend

%.install: %.mkdir
	@cd $* && $(MAKE) install

%.html-doc: %.mkdir
	@cd $* && $(MAKE) html-doc

%.doc: %.mkdir
	@cd $* && $(MAKE) doc

%.chk: %.mkdir
	@cd $* && $(MAKE) chk

chk:
	$(F95CHK) --dry-run $(FFILES:%=$(srcdir)/%)

chk-fix:
	$(F95CHK) $(FFILES:%=$(srcdir)/%)
