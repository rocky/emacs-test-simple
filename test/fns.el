(require 'cl)
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
(incf (test-info-assert-count test-info))
(incf (test-info-failure-count test-info))
(assert-matches "1 failure in 1 assertion" (test-simple-summary-line test-info)
		"handling singular correctly")
(incf (test-info-assert-count test-info))
(assert-matches "1 failure in 2 assertions" (test-simple-summary-line test-info)
		"back to plural for two assertions")

(end-tests)
