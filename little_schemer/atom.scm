;; This is a procedure defined in The Little Schemer
(define atom? 
  (lambda (x)
	(and (not (pair? x)) (not (null? x)))
  )
)
(atom? 'fred)
(atom? '())
(atom? 15)
(atom? '(1 2 3))
