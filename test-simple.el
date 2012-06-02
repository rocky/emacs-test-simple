;;; test-simple.el --- Simple Unit Test Framework for Emacs Lisp 
;; adapted from Phil Hagelberg's behave.el by rocky
;; See also Christian Ohler's ert http://github.com/ohler/ert

;; Copyright (C) 2007 Phil Hagelberg
;; Copyright (C) 2010, 2012 Rocky Bernstein

;; Author: Phil Hagelberg
;; Author: Rocky Bernstein
;; Created: 19 Jan 2007
;; Version: 0.01
;; URL: http://github.com/rocky/emacs-test-unit
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

;; test-simple.el allows you to unit tests for your Emacs Lisp
;; code. Executable specifications allow you to check that your code
;; is working correctly in an automated fashion that you can use to
;; drive the focus of your development. (It's related to Test-Driven
;; Development.) You can read up on it at http://behaviour-driven.org.

;; Specifications and contexts both must have docstrings so that when
;; the specifications aren't met it is easy to see what caused the
;; failure.  Each specification should live within a context. In each
;; context, you can set up relevant things to test, such as necessary
;; buffers or data structures. (Be sure to use lexical-let for setting
;; up the variables you need--since the specify macro uses lambdas,
;; closures will be made for those variables.) Everything within the
;; context is executed normally.

;; Each context can be tagged with the TAG form. This allows you to
;; group your contexts by tags. When you execute the specs, M-x test-unit
;; will ask you to give some tags, and it will execute all contexts
;; that match those tags.

;; When you want to run the specs, evaluate them and press M-x
;; test-simple. Enter the tags you want to run (or "all"), and they will be
;; executed with results in the *test-simple* buffer. You can also do M-x
;; specifications to show a list of all the specified behaviours of
;; the code.

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

;; See meta.el for specifications for test-simple.el. Evaluate meta.el and
;; M-x specifications meta RET to see the specifications explained.
;;; Code:

(make-variable-buffer-local (defvar spec-count))
(make-variable-buffer-local (defvar spec-desc))

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
  (lexical-let ((context (make-context)))
    (setf (context-description context) description)
    (add-to-list '*test-simple-contexts* context)))

(defmacro specify (description &rest body)
  "Add a specification and its description to the current context."
  `(push (lambda () ,description 
	   (let ((spec-desc ,description)) 
	     ,@body)) (context-specs context)))

(defmacro tag (&rest tags)
  "Give a context tags for easy reference. (Must be used within a context.)"
  `(setf (context-tags context) 
	 (append '(,@tags) (context-tags context))))

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
	(signal 'test-simple-spec-failed 
		(format 
		 "Context: %s%s\n" 
		 context-mess fail-message))))
  t)

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
  (message "Test-Simple: contexts cleared"))

(defun context-find (description)
  "Find a context by its description."
  (find description *test-simple-contexts* 
	:test (lambda (description context) (equal description (context-description context)))))

(defun context-find-by-tag (tag)
  (remove-if (lambda (context) (not (find tag (context-tags context))))
	     *test-simple-contexts*))

(defun context-find-by-tags (tags)
  (if (find 'all tags)
      *test-simple-contexts*
    (delete nil (remove-duplicates (mapcan 'context-find-by-tag tags)))))

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
      (test-simple-describe-failures failures start-time))
    (if noninteractive 
	(progn 
	  (switch-to-buffer "*test-simple*")
	  (message "%s" (buffer-substring (point-min) (point-max)))))
    (length failures)))

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
  (princ (concat "\n\n" (number-to-string (length failures)) " problem" (unless (= 1 (length failures)) "s") " in " 
		 (number-to-string spec-count)
		 " specification" (unless (= 1 spec-count) "s") 
		 " using " (number-to-string *test-simple-total-assertions*) " assertions. "
		 "(" (number-to-string (- (cadr (current-time)) start-time)) " seconds)\n\n"))
  (dolist (failure failures)
    (test-simple-report-result failure)))

(defun test-simple-report-result (failure)
  (princ failure)
  (princ "\n\n"))

(defun specifications (&optional tags)
  "Show specifications for all contexts that match given tags"
  (interactive)
  (let ((tags-string (or tags (read-string (concat "Show specs matching these tags (default " *test-simple-default-tags* "): ")
					   nil nil *test-simple-default-tags*))))
    (with-output-to-temp-buffer "*test-simple*"
      (princ "Specifications:\n")
      (mapcar #'specify-context (context-find-by-tags (mapcar 'intern (split-string tags-string " ")))))))

(defun specify-context (context)
  (princ (concat "\n" (context-description context) "...\n"))
  (dolist (spec (context-specs context))
    (princ (concat " * " (caddr spec) "\n"))))

(provide 'test-simple)
;;; test-simple.el ends here
