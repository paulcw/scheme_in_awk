(define makeaddton (lambda (n) (lambda (x) (+ x n))))
(define addtofive (makeaddton 5))
(print (addtofive 19))
