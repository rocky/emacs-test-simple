include $(top_srcdir)/common.mk

cask_test_files := test-basic.el test-fns.el test-no-clear.el

EL_GET_CHECK_FILES = $(notdir $(cask_test_files:.el=.elrun))

PHONY=check test all check-elget test-elget help
EXTRA_DIST=gcd.py gcd.rb

all:

#: same thing as "check"
test: check

test_files := $(wildcard test-*.el)

CHECK_FILES = $(notdir $(test_files:.el=.run))

#: Run all tests
check: $(CHECK_FILES)

#: Run all tests with minimum verbosity
check-short:
	$(MAKE) check 2>&1  | ruby ../make-check-filter.rb

install-lispLISP: $(lisp_LISP) $(ELCFILES)

test-%.run:
	(cd $(top_srcdir)/test && EMACSLOADPATH=$(EMACSLOADPATH) $(EMACS) --batch --no-site-file --no-splash --load $(@:.run=.el))

#: Run tests
test-%.elrun:
	(cd $(top_srcdir)/test && $(EMACS) --batch --no-site-file --no-splash --load $(@:.elrun=.el))

# Whatever it is you want to do, it should be forwarded to the
# to top-level directories
%:
	$(MAKE) -C .. $@
