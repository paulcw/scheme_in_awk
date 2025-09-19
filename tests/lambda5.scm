(define foo (lambda (x y z) (print x y z)))
(foo 1 2 3)
;; => (1 2 3)
;;(foo 1)
;; ^^ makes an error, not enough args
;;(foo 1 2 3 4)
;; ^^ makes an error, too many args
;;(foo)
;; ^^ makes an error, not enough (none) args
