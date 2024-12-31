(define (over-or-under num1 num2) 
	(cond
		((< num1 num2) -1)
		((= num1 num2) 0)
		(else 1)
	)
)

(define (make-adder num) 
	(lambda (x) (+ num x))
)

(define (composed f g) 
	(lambda (x) (f (g x)))
)

(define (square n) (* n n))

(define (pow base exp) 
	(if (= (modulo exp 2) 0)
		(if (= exp 2) (square base) (square (pow base (/ exp 2))))
		(* base (pow base (- exp 1)))
	)
)
