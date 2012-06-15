(require 'cl)
(load-file "../test-simple.el")
;; We don't do this or test-simple-start
;; (test-simple-clear)

(note "no-test-start")
(assert-t (memq 'test-simple features) "'test-simple provided")

(assert-nil nil)

(end-tests)
