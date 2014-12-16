dnl FIXME: pick up from test-simple.el
AC_INIT(emacs-test-simple, 1.0,)
AC_CONFIG_SRCDIR(test-simple.el)
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE

AC_PATH_PROG([EMACS], [emacs], [emacs])
AC_MSG_NOTICE("Checking emacs version and prerequiste packages")
$EMACS -batch -q -no-site-file -eval \
  '(if (<= emacs-major-version 22)
       (progn
         (error "You need GNU Emacs 23 or better.")
         (kill-emacs 1)
       )
   )'
if test $? -ne 0 ; then
    AC_MSG_ERROR([Can't continue until above error is corrected.])
fi

##################################################################
# See if --with-lispdir was set. If not, set it to a reasonable default
# based on where bash thinks bashdb is supposed to be installed.
##################################################################

AM_MISSING_PROG(GIT2CL, git2cl, $missing_dir)

# Check whether --with-lispdir was given.
if test "${with_lispdir+set}" = set -o "${prefix+set}" = set; then :
else
  my_lispdir=$(EMACS_PROG=$EMACS $SH_PROG $(dirname $0)/compute-lispdir.sh)
  if test "${my_lispdir+set}" = set; then :
    with_lispdir=$my_lispdir
    echo "'compute-lispdir.sh' lispdir install directory override: '$with_lispdir'"
  fi
fi

##
## Find out where to install the debugger emacs lisp files
##
AM_PATH_LISPDIR
AM_CONDITIONAL(INSTALL_EMACS_LISP, test "x$lispdir" != "x")

AC_CONFIG_FILES([Makefile test/Makefile])
AC_OUTPUT
