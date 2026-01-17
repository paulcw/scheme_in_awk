(define dog 15)
(letrec
  (
	(dog (lambda (x) (if (zero? x) x (+ x (dog (- x 1))))))
	(cat 9)
  )
  (print (dog cat))
)
