;;; test-simple.el --- Simple Unit Test Framework for Emacs Lisp
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
