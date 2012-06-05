lisp_files := $(wildcard *.el)
test_files := $(wildcard test/*.el)

EXTRA_DIST = $(lisp_files) $(test_files) README README.textile COPYING

CHECK_FILES = $(notdir $(test_files:.el=.run))

lisp_LISP = $(lisp_files)

check: $(CHECK_FILES)

check-short:
	$(MAKE) check 2>&1  | ruby make-check-filter.rb


README: README.textile
	cp README.textile README

# FIGURE out how to DRY the run rules.
basic.run:
	(cd $(srcdir)/test && $(EMACS) -batch --no-site-file --no-splash --no-load -l $(@:.run=.el) )

# FIGURE out how to DRY the run rules.
fns.run:
	(cd $(srcdir)/test && $(EMACS) -batch --no-site-file --no-splash --no-load -l $(@:.run=.el) )

.PHONY: $(CHECK_FILES)

if MAINTAINER_MODE

CL = ChangeLog
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

test: check
