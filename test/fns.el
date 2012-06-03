(require 'cl)
(load-file "../test-simple.el")
(test-simple-clear)

(note "Initializing test information")
(setq test-info (make-context))
(test-simple-clear test-info)
(assert-equal 0 (context-assert-count test-info) "Count zeroed")
(assert-equal 0 (context-failure-count test-info) "Failure zeroed")

(note "Summary information")
(assert-matches "0 failures over 0 assertions" (test-simple-summary-line test-info))

(end-tests)
