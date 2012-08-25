lisp_files := $(wildcard *.el)
lisp_LISP = $(lisp_files)
test_files := $(wildcard test/*.el)

EXTRA_DIST = $(lisp_files) $(test_files) README README.textile COPYING

CHECK_FILES = $(notdir $(test_files:.el=.run))


check: $(test-files)
	$(MAKE) -C test check

check-short:
	$(MAKE) -C test check 2>&1  | ruby make-check-filter.rb


README: README.textile
	cp README.textile README

.PHONY: $(CHECK_FILES)

if MAINTAINER_MODE

CL = ChangeLog
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

test: check
