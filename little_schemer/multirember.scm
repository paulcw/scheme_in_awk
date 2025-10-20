
; removes all instances of a
(define multirember (lambda (a lat)
	(cond
		((null? lat) '())
		((eq? a (car lat)) (multirember a (cdr lat)))
		(else (cons (car lat) (multirember a (cdr lat)))))))
(multirember 'bacon '(bacon lettuce and tomato))
(multirember 'bacon '(lettuce bacon and tomato))
(multirember 'bacon '(lettuce bacon and tomato bacon bacon))
(multirember 'bacon '(lettuce cheese and tomato))
(multirember 'bacon '())
