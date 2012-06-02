;;; test-simple.el --- Simple Unit Test Framework for Emacs Lisp 
;; adapted from Phil Hagelberg's behave.el by rocky
;; See also Christian Ohler's ert http://github.com/ohler/ert

;; Copyright (C) 2010, 2012 Rocky Bernstein

;; Author: Rocky Bernstein
;; URL: http://github.com/rocky/emacs-test-simple
;; Keywords: unit-test specification specs

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; test-simple.el allows you to write tests for your Emacs Lisp
;; code. Executable specifications allow you to check that your code
;; is working correctly in an automated fashion that you can use to
;; drive the focus of your development. (It's related to Test-Driven
;; Development.) You can read up on it at http://behaviour-driven.org.

;; Assertions may have docstrings so that when the specifications
;; aren't met it is easy to see what caused the failure.  

;; When "note" is used subsequent tests are grouped assumed to be
;; related to that not.

;; When you want to run the specs, evaluate the buffer. Or evaluate
;; individual assertions. Results are save in the
;; *test-simple* buffer. 

;;; Implementation

;; Contexts are stored in the *test-simple-contexts* list as structs. Each
;; context has a "specs" slot that contains a list of its specs, which
;; are stored as closures. The expect form ensures that expectations
;; are met and signals test-simple-spec-failed if they are not.

;; Warning: the variable CONTEXT is used within macros
;; in such a way that they could shadow variables of the same name in
;; the code being tested. Future versions will use gensyms to solve
;; this issue, but in the mean time avoid relying upon variables with
;; those names.

;;; To do:

;; Main issues: more expect predicates

;;; Usage:

(make-variable-buffer-local (defvar spec-count))
(make-variable-buffer-local (defvar spec-desc))
(make-variable-buffer-local (defvar test-simple-failures nil))

(eval-when-compile 
  (byte-compile-disable-warning 'cl-functions)
  ;; Somehow disabling cl-functions causes the erroneous message:
  ;;   Warning: the function `reduce' might not be defined at runtime.
  ;; FIXME: isolate, fix and/or report back to Emacs developers a bug
  (byte-compile-disable-warning 'unresolved)
  (require 'cl)
  )
(require 'cl)

(make-variable-buffer-local 
 (defvar *test-simple-contexts* '()
   "A list of contexts and their specs."))

(make-variable-buffer-local
 (defvar *test-simple-default-tags* "all"))

(defvar test-simple-debug-on-error nil
  "If non-nil raise an error on the first failure")

(make-variable-buffer-local
 (defvar *test-simple-total-assertions* 0
  "Count of number of assertions seen since the last `test-simple-clear-contexts'"
  ))

(defstruct context 
  description
  tags 
  (specs '()) ;; list of its specifications stored as closures.
  refreshing-vars)

(put 'test-simple-spec-failed 'error-conditions '(failure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note (description)
  "Defines a context for specifications to run in."
  (setf (context-description context) description)
  (add-to-list '*test-simple-contexts* context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assertion tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assert-raises (error-condition body &optional opt-fail-message)
  (let ((fail-message (or opt-fail-message
			  (format "assert-raises did not get expected %s" 
				  error-condition))))
    (list 'condition-case nil
	  (list 'progn body
		(list 'assert-t nil fail-message))
	  (list error-condition '(assert-t t)))))

(defun assert-equal (expected actual &optional opt-fail-message)
  "expectation is that ACTUAL should be equal to EXPECTED."
  (if (boundp '*test-simple-total-assertions*)
      (incf *test-simple-total-assertions*))
  (if (not (equal actual expected))
      (let* ((fail-message 
	      (if opt-fail-message
		  (format "\n\tMessage: %s" opt-fail-message)
		""))
	     (context-mess 
	      (if (boundp 'context)
		  (context-description context)
		"unset")))
	(signal 'test-simple-spec-failed 
		(format 
		 "Context: %s%s\n\n\tExpected: %s\n\tGot:      %s"
		 context-mess
		 fail-message expected actual))))
  t)

(defun assert-t (actual &optional opt-fail-message)
  "expectation is that ACTUAL is not nil."
  (assert-nil (not actual) opt-fail-message))

(defun assert-nil (actual &optional opt-fail-message)
  "expectation is that ACTUAL is nil."
  (incf *test-simple-total-assertions*)
  (if actual
      (let* ((fail-message 
	      (if opt-fail-message
		  (format "\n\tMessage: %s" opt-fail-message)
		""))
	     (context-mess 
	      (if (boundp 'context)
		  (context-description context)
		"unset")))
	(add-failure "assert-nil" context-mess fail-message))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-simple-clear-contexts ()
  (interactive)
  (setq *test-simple-contexts* '())
  (setq *test-simple-total-assertions* 0)
  (make-variable-buffer-local (defvar context (make-context)))
  (setf (context-description context) "no description set")
  (setf (context-specs context) '())
  (with-current-buffer (get-buffer-create "*test-simple*")
    (let ((old-read-only inhibit-read-only))
      (setq inhibit-read-only 't)
      (delete-region (point-min) (point-max))
      (setq inhibit-read-only old-read-only)))
  (message "Test-Simple: contexts cleared"))

(defun add-failure(type context-msg fail-msg)
  (let ((failure-line
	 (format "Context: %s, type %s %s" context-msg type fail-msg))
	(old-read-only inhibit-read-only)
	)
    (save-excursion
      (princ "F")
      (switch-to-buffer "*test-simple*")
      (setq inhibit-read-only 't)
      (insert (concat failure-line "\n"))
      (overlay-put (make-overlay (point) (- (point) 1)) 
		   'face '(foreground-color . "red"))
      (add-to-list 'test-simple-failures failure-line)
      (setq inhibit-read-only old-read-only)
      (switch-to-buffer nil))
    (unless noninteractive
      (if test-simple-debug-on-error
	  (signal 'test-simple-assert-failed failure-line)
	(message failure-line)
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-simple (&optional tags)
  "Execute all contexts that match given tags"
  (interactive)
  (let ((tags-string (or tags (read-string (concat "Execute specs matching these tags (default " *test-simple-default-tags* "): ")
					   nil nil *test-simple-default-tags*)))
	(start-time (cadr (current-time)))
	(failures nil)
	(spec-count 0))
    (setq *test-simple-default-tags* tags-string) ; update default for next time
    (with-output-to-temp-buffer "*test-simple*"
      (princ (concat "Running specs tagged \"" tags-string "\":\n\n"))
      (dolist (context (context-find-by-tags (mapcar 'intern (split-string tags-string " "))))
	(execute-context context))
      (test-simple-describe-failures test-simple-failures start-time))
    (if noninteractive 
	(progn 
	  (switch-to-buffer "*test-simple*")
	  (message "%s" (buffer-substring (point-min) (point-max)))))
    (length test-simple-failures)))

(defun execute-context (context)
  (condition-case failure
      (mapcar #'execute-spec (reverse (context-specs context)))
    (error (princ "E")
	   (switch-to-buffer "*test-simple*")
	   (overlay-put (make-overlay (point) (- (point) 1)) 'face '(foreground-color . "red"))
	   (switch-to-buffer nil)
	   (add-to-list 'failures (list "Error:" failure) t))
    (failure (princ "F")
	     (switch-to-buffer "*test-simple*")
	     (overlay-put (make-overlay (point) (- (point) 1)) 'face '(foreground-color . "red"))
	     (switch-to-buffer nil)
	     (add-to-list 'failures (cdr failure) t))))

(defun execute-spec (spec)
  (incf spec-count)
  (funcall spec)
  (princ "."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-simple-describe-failures (failures start-time)
  (princ (concat "\n\n" (number-to-string (length failures)) " problem" 
		 (unless (= 1 (length failures)) "s") " in " 
		 (number-to-string spec-count)
		 " specification" (unless (= 1 spec-count) "s") 
		 " using " (number-to-string *test-simple-total-assertions*) " assertions. "
		 "(" (number-to-string (- (cadr (current-time)) start-time)) " seconds)\n\n"))
  (dolist (failure failures)
    (test-simple-report-result failure)))

(defun test-simple-report-result (failure)
  (princ failure)
  (princ "\n\n"))

(provide 'test-simple)
;;; test-simple.el ends here
