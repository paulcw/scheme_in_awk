(define insertL (lambda (new target lst)
	(cond
		((null? lst) '())
		((eq? target (car lst)) (cons new lst))
		(else (cons (car lst) (insertL new target (cdr lst)))))))

(insertL 'a 'b '(c b d e))
(insertL 'a 'b '())
(insertL 'a 'b '(g b))
(insertL 'a 'b '(b g b))
