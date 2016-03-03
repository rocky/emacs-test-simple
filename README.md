[![Build Status](https://travis-ci.org/rocky/emacs-test-simple.png)](https://travis-ci.org/rocky/emacs-test-simple)

*test-simple.el* is :

* Simple -- no need for context macros, enclosing specifications, or required test tags. But if you want, you still can add custom assert failure messages or add notes before a group of tests.
* Accomodates both interactive and non-interactive use:
  * For interactive use one can use `eval-last-sexp`, `eval-region`, and `eval-buffer`
  * For non-interactive use run as: `emacs --batch --no-site-file --no-splash --load <test-lisp-code.el>`

I use this in my [Debugger front end](https://github.com/rocky/emacs-dbgr).

Here is an example found in the [examples directory](https://github.com/rocky/emacs-test-simple/tree/master/test).

In file `gcd.el`:

    (defun gcd(a b)
      "Greatest Common Divisor of A and B"
      ;; Make a < b
      (if (> a b)
          (let ((c a))
    	(setq a b)
    	(setq b c)))
      (cond
       ((< a 0) nil)
       ((or (= 0 (- b a)) (= a 1)) a)
       (t (gcd (- b a) a))
       )
    )


In file `test-gcd.el` in the same directory:

    (require 'test-simple)
    (test-simple-start) ;; Zero counters and start the stop watch.

    ;; Use (load-file) below because we want to always to read the source.
    ;; Also, we don't want no stinking compiled source.
    (assert-t (load-file "./gcd.el")
    	  "Can't load gcd.el - are you in the right directory?" )

    (note "degenerate cases")

    (assert-nil (gcd 5 -1) "using positive numbers")
    (assert-nil (gcd -4 1) "using positive numbers, switched order")
    (assert-raises error (gcd "a" 32)
    	       "Passing a string value should raise an error")

    (note "GCD computations")
    (assert-equal 1 (gcd 3 5) "gcd(3,5)")
    (assert-equal 8 (gcd 8 32) "gcd(8,32)")

    (end-tests) ;; Stop the clock and print a summary

Edit (with Emacs of course) `test-gcd.el` and run `M-x eval-current-buffer`

You should see in buffer `*test-simple*`:

    test-gcd.el
    ......
    0 failures in 6 assertions (0.002646 seconds)

Now let's try from a command line:

    $ emacs --batch --no-site-file --no-splash --load test-gcd.el
    Loading /src/external-vcs/emacs-test-simple/example/gcd.el (source)...
    *scratch*
    ......
    0 failures in 6 assertions (0.000723 seconds)

You can run noninteractive tests inside Emacs by `test-simple-run`.
Add the following at a test file:

    ;;;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

Press C-x C-e at the `test-simple-run` line to run this test file.
Then press C-x C-z, which is customizable by `test-simple-runner-key`, to run it more.
If you have installed `bpr` package, use it by default because it only pops up window when the running program exits abnormally.

`test-simple-run` can be called interactively.
In this case, the command line is set above as the simplest case.
But you run test with dependency, you must use the sexp comment form.

    ;;;; (test-simple-run "emacs -batch -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "foo")) buffer-file-name)

*Author:*  Rocky Bernstein <rocky@gnu.org> <br>
[![endorse](https://api.coderwall.com/rocky/endorsecount.png)](https://coderwall.com/rocky)
