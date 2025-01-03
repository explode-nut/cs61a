(define (cddr s) (cdr (cdr s)))

(define (cadr s) 
	(car (cdr s))
)

(define (caddr s) 
	(car (cddr s))
)

(define (ascending? lst) 
	(cond
		((null? (cdr lst)) #t)
		((> (cadr lst) (car lst)) (ascending? (cdr lst)))
		((= (cadr lst) (car lst)) (ascending? (cdr lst)))
		((< (cadr lst) (car lst)) #f)
	)
)

(define (h lst1 lst2 mark)
	(cond
		((null? lst1) lst2)
                ((null? lst2) lst1)
                ((= mark 2) (cons (car lst1) (h (cdr lst1) lst2 1)))
                ((= mark 1) (cons (car lst2) (h lst1 (cdr lst2) 2)))
        )
)
(define (interleave lst1 lst2)
	(h lst1 lst2 2)
)

(define (my-filter func lst) 
	(cond
		((null? lst) nil)
		((func (car lst)) (cons (car lst) (my-filter func (cdr lst))))
		(else (my-filter func (cdr lst)))
	)
)

(define (no-repeats lst) 
	(cond 
		((null? lst) lst)
		(else (cons 
			(car lst)
			(no-repeats (my-filter (lambda (x) (not (= (car lst) x))) (cdr lst)))))
	)
)
