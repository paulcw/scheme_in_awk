(define insertR (lambda (new target lst)
	(cond
		((null? lst) '())
		((eq? target (car lst)) (cons (car lst) (cons new (cdr lst))))
		(else (cons (car lst) (insertR new target (cdr lst)))))))

(insertR 'a 'b '(c b d e))
(insertR 'a 'b '())
(insertR 'a 'b '(g b))
