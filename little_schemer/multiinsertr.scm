(define multiinsertR (lambda (new target lst)
	(cond
		((null? lst) '())
		((eq? target (car lst))
		 (cons (car lst) (cons new (multiinsertR new target (cdr lst)))))
		(else (cons (car lst) (multiinsertR new target (cdr lst)))))))

(multiinsertR 'a 'b '(c b d e))
(multiinsertR 'a 'b '(c b d b e))
(multiinsertR 'a 'b '())
(multiinsertR 'a 'b '(g b))
(multiinsertR 'a 'b '(g a))
