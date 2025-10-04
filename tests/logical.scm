(or 1 2 3) ; => #t
(or 0 1 2) ; => #t
(or 'dog 'cat #f) ; => #t
(or #t #f) ; => #t
(or #f #f) ; => #f
(or #t #t) ; => #t
(or #f) ; => #f
(or #t) ; => #t
(or) ; => #f

(and 1 2 3) ; => #t
(and 0 1 2) ; => #t
(and 'dog 'cat #f) ; => #f
(and #t #f) ; => #f
(and #f #f) ; => #f
(and #t #t) ; => #t
(and #f) ; => #f
(and #t) ; => #f
(and) ; => #t

(not #f) ; => #t
(not #t) ; => #f
