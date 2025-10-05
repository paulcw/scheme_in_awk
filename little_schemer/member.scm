
(define member?
	(lambda (a lat)
		(cond
			((null? lat) #f)
			(else (or (eq? (car lat) a)
				  	  (member? a (cdr lat)))))))  

(member? 'meat '(mashed potatoes and meat gravy))
(member? 'meat '(mashed potatoes and vegan gravy))

