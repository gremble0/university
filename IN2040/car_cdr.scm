(define (cons x y)
  (lambda (z) (z x y)))

(define (car p)
  (p (lambda (x y) x)))

(define (cdr p)
  (p (lambda (x y) y)))

(define foo (cons 42 (cons 3 '())))
