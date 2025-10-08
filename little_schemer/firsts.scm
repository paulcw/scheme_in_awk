(define firsts (lambda (lst)
	(cond
		((null? lst) (quote ()))
		(else (cons (car (car lst))
			        (firsts (cdr lst)))))))

(firsts '((apple peach pumpkin)
			(plum pear cherry)
			(grape raisin pea)
			(bean carrot eggplant)))
