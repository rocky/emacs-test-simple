# Note: This makefile include remake-style target comments.
# These comments before the targets start with #:
# remake --tasks to shows the targets and the comments

GIT2CL ?= git2cl
RUBY   ?= ruby

lisp_files := $(wildcard *.el)
lisp_LISP = $(lisp_files)
test_files := $(wildcard test/*.el)

EXTRA_DIST = $(lisp_files) $(test_files) README THANKS README.md COPYING

CHECK_FILES = $(notdir $(test_files:.el=.run))


check: $(test-files)
	$(MAKE) -C test check

README: README.textile
	ln -s README.md README

PHONY=check clean dist distclean test check-short check-terse install-short

if MAINTAINER_MODE

ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

#: Run all tests
test: check

check-short:
	$(MAKE) -C test check 2>&1  | ruby make-check-filter.rb

#: Run all tests without and show just the failure lines
check-terse:
	$(MAKE) check 2>&1  | $(RUBY) make-check-filter.rb | grep failure

#: Run "make install"
install-short:
	$(MAKE) install 2>&1  | $(RUBY) make-check-filter.rb
