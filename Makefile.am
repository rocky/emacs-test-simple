# Note: This makefile include remake-style target comments.
# These comments before the targets start with #:
# remake --tasks to shows the targets and the comments

GIT2CL ?= git2cl
RUBY   ?= ruby

lisp_files := $(wildcard *.el)
lisp_LISP = $(lisp_files)
test_files := $(wildcard test/*.el)

EXTRA_DIST = $(lisp_files) $(test_files) README THANKS README.md

CHECK_FILES = $(notdir $(test_files:.el=.run))


check: $(test-files)
	$(MAKE) -C test check

README: README.textile
	ln -s README.md README

PHONY=check check_copyrights clean dist distclean test check-short check-terse install-short

if MAINTAINER_MODE

ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

#: Run all tests
test: check

#: Run all tests with reduced versbosity
check-short:
	$(MAKE) -C test check 2>&1  | ruby make-check-filter.rb

#: Run tests showing only the failure lines; See also check-short
check-terse:
	$(MAKE) check 2>&1  | $(RUBY) make-check-filter.rb | grep failure

#: Run "make install" with reduced verbosity
install-short:
	$(MAKE) install 2>&1  | $(RUBY) make-check-filter.rb

CR_EXCEPTIONS=copyright_exceptions
#: Check for GNU Copyrights.
check_copyrights:
	@echo "Compute exceptions >$(CR_EXCEPTIONS)~"
	@export LANG=C;							\
	find . -name '.git' -prune -o -name '*.el' -print0 |		\
	    xargs -0 grep -L 'Free Software Foundation, Inc' |		\
	    grep -v '\(\.dir-locals\|.-\(pkg\|autoloads\)\)\.el$$';	\
	find . -name '.git' -prune -o -name '*.el' -print |		\
	    while read f; do						\
	        fquoted="$$(echo $$f|tr '|' '_')";			\
	        sed -n -e '/[Cc]opyright.*, *[1-9][-0-9]*,\?$$/N'	\
	            -e '/Free Software Foundation/d'			\
	            -e "s|^\\(.*[Cc]opyright\\)|$$fquoted:\\1|p" 	\
	           "$$f";						\
	    done | sort >$(CR_EXCEPTIONS)~
	diff -u "$(CR_EXCEPTIONS)" "$(CR_EXCEPTIONS)~"
