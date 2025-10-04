; "lat?" is a function used as an example in The Little Schemer
(define lat?
	(lambda (x)
		(cond
			((null? x) #t)
			((atom? (car x)) (lat? (cdr x)))
			(else #f))))
(lat? '(bacon and eggs))
(lat? '(bacon (and eggs)))
