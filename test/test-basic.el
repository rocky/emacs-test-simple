;;; test-simple.el --- Simple Unit Test Framework for Emacs Lisp
;; Copyright (C) 2015 Free Software Foundation, Inc
;; Author: Rocky Bernstein <rocky@gnu.org>
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
(require 'cl-lib)
(load-file "../test-simple.el")
(test-simple-start "test-simple.el")

(note "basic-tests")
(assert-t (memq 'test-simple features) "'test-simple provided")

(assert-nil nil "Knights of ni")
(assert-equal 5 (+ 1 4) "assert-equal")
(assert-raises error (error "you should not see this") "assert-raises")

(end-tests)
