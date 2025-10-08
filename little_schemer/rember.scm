
(define rember (lambda (a lat)
	(cond
		((null? lat) '())
		((eq? a (car lat)) (cdr lat))
		(else (cons (car lat) (rember a (cdr lat)))))))
(rember 'bacon '(bacon lettuce and tomato))
(rember 'bacon '(lettuce bacon and tomato))
(rember 'bacon '(lettuce cheese and tomato))
(rember 'bacon '())
