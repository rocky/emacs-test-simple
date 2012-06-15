(require 'cl)
(load-file "../test-simple.el")
(test-simple-clear)

(note "basic-tests")
(assert-t (memq 'test-simple features) "'test-simple provided")

(assert-nil nil "assert-nil failure test")
(assert-nil nil "Knights if ni")
(assert-equal 5 (+ 1 4) "assert-equal")
(assert-raises error (error "you should not see this") "assert-raises")

(end-tests)
