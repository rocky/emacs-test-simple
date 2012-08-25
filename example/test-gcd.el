(require 'test-simple)

(test-simple-start)

(assert-t (load-file "./gcd.el") 
	  "Can't load gcd.el - are you in the right directory?" )

(note "degenereate cases")

(assert-nil (gcd 5 -1) "using positive numbers")
(assert-nil (gcd -4 1) "using positive numbers, switched order")

(note "GCD computations")
(assert-equal 1 (gcd 3 5) "gcd(3,5)")
(assert-equal 8 (gcd 8 32) "gcd(8,32)")

(assert-raises error (gcd "a" 32) 
	       "Passing a string value should raise an error")

(end-tests)
