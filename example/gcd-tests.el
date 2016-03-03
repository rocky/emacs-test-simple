;;; gcd-tests.el
;; Copyright (C) 2015 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; URL: http://github.com/rocky/emacs-test-simple
;; Keywords: unit-test
;; Version: 1.0

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)
(require 'test-simple)

(test-simple-start)

(assert-t (load-file "./gcd.el")
	  "Can't load gcd.el - are you in the right directory?" )

(note "degenereate cases")

(assert-nil (gcd 5 -1) "using positive numbers")
(assert-nil (gcd -4 1) "using positive numbers, switched order")

(note "GCD computations")
(assert-equal 1 (gcd 3 5) "gcd(3,5)")
(assert-equal 8 (gcd 8 32) "gcd(8,32)")

(assert-raises error (gcd "a" 32)
	       "Passing a string value should raise an error")

(end-tests)
