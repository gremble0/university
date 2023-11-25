#lang racket

;; 1a
(define (sign-if x)
  (if (positive? x)
    1
    (if (negative? x)
      -1
      0)))

(define (sign-and-or x)
  (or (and (positive? x) 1)
      (and (negative? x) -1)
      0))

(define (add1 x)
  (+ 1 x))

(define (sub1 x)
  (- 1 x))

(define (plus x y)
  (if (zero? x)
    y
    (plus (sub1 x) (add1 y))))

(define (minus x y)
  (if (zero? y)
    x
    (minus (sub1 x) (sub1 y))))

(define (plus-rec x y)
  (if (zero? x)
    y
    (add1 (plus-rec (sub1 x) y))))

(define (minus-rec x y)
  (if (zero? y)
    x
    (sub1 (minus-rec x (sub1 y)))))

(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
      e
      (power-iter (+ 1 e))))
  (power-iter 1))

;; 1b
((lambda (l) (cadr l)) '(0 42 #t bar))
((lambda (l) (cadar l)) '((0 42) (#t bar)))
((lambda (l) (caadr l)) '((0) (42 #t (bar))))

(list '(0 42) '(#t bar))
(cons (cons 0 (cons 42 '())) (cons (cons #t (cons 'bar '())) '()))

(define (take-rec n items)
  (if (= n 0)
    '()
    (cons (car items)
          (take-rec (- n 1)
                (cdr items)))))

(define (take-iter n items)
  (define (take-iter-impl n rest acc)
    (if (= n 0)
      (reverse acc)
      (take-iter-impl (- n 1)
                 (cdr rest)
                 (cons (car rest) acc))))
  (take-iter-impl n items '()))

(define (take-while pred items)
  (let ((cur (car items)))
    (if (pred cur)
      (cons cur (take-while pred (cdr items))) 
      '())))

(define (map2 proc lst1 lst2)
    (if (or (null? lst1)
            (null? lst2))
      '()
      (cons (proc (car lst1)
                  (car lst2))
            (map2 proc (cdr lst1) (cdr lst2)))))

(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))
