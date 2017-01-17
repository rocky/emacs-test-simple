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
(test-simple-clear)

(setq test-info (make-test-info))
(test-simple-clear test-info)

(note "Initializing test information")
(assert-equal 0 (test-info-assert-count test-info) "Count zeroed")
(assert-equal 0 (test-info-failure-count test-info) "Failure zeroed")

(note "Summary information")
(assert-matches "0 failures in 0 assertions" (test-simple-summary-line test-info)
		"initial summary")
(cl-incf (test-info-assert-count test-info))
(cl-incf (test-info-failure-count test-info))
(assert-matches "1 failure in 1 assertion" (test-simple-summary-line test-info)
		"handling singular correctly")
(cl-incf (test-info-assert-count test-info))
(assert-matches "1 failure in 2 assertions" (test-simple-summary-line test-info)
		"back to plural for two assertions")

(end-tests)
