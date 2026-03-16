;; tests for the quasiquote mechanism, stolen from rsr7 text
;; YOU'LL NOTE THAT SOME ARE COMMENTED OUT, THIS ISN'T DONE

`(list ,(+ 1 2) 4)
;; ⇒ (list 3 4)

(let ((name 'a)) `(list ,name ',name))
;; ⇒ (list a (quote a))

;`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
;;⇒ (a 3 4 5 6 b)

`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
;; ⇒ ((foo 7) . cons)

;`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
;; ⇒ #(10 5 2 4 3 8)

(let ((foo '(foo bar)) (@baz 'baz))
`(list ,@foo , @baz))
;; ⇒ (list foo bar baz)


; Quasiquote expressions can be nested. Substitutions are
; made only for unquoted components appearing at the same
; nesting level as the outermost quasiquote. The nesting
; level increases by one inside each successive quasiquotation,
; and decreases by one inside each unquotation.

`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
;; ⇒ (a `(b ,(+ 1 2) ,(foo 4 d) e) f)

(let ((name1 'x)
(name2 'y))
`(a `(b ,,name1 ,',name2 d) e))
;; ⇒ (a `(b ,x ,'y d) e)
