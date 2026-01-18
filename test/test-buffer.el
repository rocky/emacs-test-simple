;;; test-simple.el --- Simple Unit Test Framework for Emacs Lisp
;; Copyright (C) 2023 Free Software Foundation, Inc
;; Author: 813gan
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

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)


(load-file "../test-simple.el")
(test-simple-start "test-simple.el")

(note "make sure test-simple don't flush buffer")
(with-temp-buffer
  (insert "test")
  (assert-t 't)
  (assert-nil (equal (point-min) (point-max)) "test-simple is breaking buffers.") )

(end-tests)
