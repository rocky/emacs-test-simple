AC_INIT(emacs-test-simple, 0.02vc,)
AC_CONFIG_SRCDIR(test-simple.el)
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE
AM_MISSING_PROG(GIT2CL, git2cl, $missing_dir)

AC_PATH_PROG([EMACS], [emacs], [emacs])
$EMACS -batch -q -no-site-file -eval \
  '(if (<= emacs-major-version 22)
     (progn
       (error "You need GNU Emacs 23 or better.")
       (kill-emacs 1)
     ))'
if test $? -ne 0 ; then
    AC_MSG_ERROR([Can't continue with this version of Emacs])
fi

##
## Find out where to install the debugger emacs lisp files
##
AM_PATH_LISPDIR
AM_CONDITIONAL(INSTALL_EMACS_LISP, test "x$lispdir" != "x")

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
