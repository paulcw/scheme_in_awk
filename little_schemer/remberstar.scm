
(define rember* (lambda (a lat)
	(cond
		((null? lat) '())
		((pair? (car lat)) (cons (rember* a (car lat)) (rember* a (cdr lat))))
		((eq? a (car lat)) (rember* a (cdr lat)))
		(else (cons (car lat) (rember* a (cdr lat)))))))
; previous tests for rember would not necessarily still work,
; because it only removed the first, top-level instance.
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
; -> ((coffee) ((tea)) (and (hick)))

