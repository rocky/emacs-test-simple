(require 'cl)
(load-file "../test-simple.el")
(test-simple-clear-contexts)

(note "basic-tests")
(assert-t (memq 'test-unit features) "'test-unit provided")

(assert-nil 't "assert-nil failure test")
(assert-nil nil "Knights if ni")
(assert-equal 5 (+ 1 4) "assert-equal")
(assert-raises error (error "you should not see this") "assert-raises")

(test-simple "basic-tests")


