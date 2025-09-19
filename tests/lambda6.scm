(define foo (lambda a (print a)))
(foo 5)
(foo 3 2 1)
(foo)

;; third kind of formal param list:
(define foo2 (lambda (x . z) (print "x is " x "and z is " z)))
;;(foo2)
;; should fail above, following fine?
(foo2 9)
(foo2 9 2 3)
