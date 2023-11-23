#lang racket

;; Recursive process
(define (fac-rec n)
  (if (= n 1)
      1
      (* n (fac-rec (- n 1)))))

;; Iterative process
(define (fac-iter n)
  (define (fac-iter-impl n acc)
    (if (= n 0)
        acc
        (fac-iter-impl (- n 1)
                       (* n acc))))
  (fac-iter-impl n 1))

;; Three-recursive process
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

;; Iterative process
(define (fib-iter n)
  (define (fib-iter-impl next cur count)
    (if (= count n)
        cur
        (fib-iter-impl (+ next cur)
                       next
                       (+ count 1))))
  (fib-iter-impl 1 0 0))

;; Recursive process
(define (exponent x n)
  (if (= n 0)
      1
      (* x (exponent x (- n 1)))))

;; Iterative process
(define (exponent-iter x n)
  (define (exponent-iter-impl acc n)
    (if (= n 0)
        acc
        (exponent-iter-impl (* x acc)
                       (- n 1))))
  (exponent-iter-impl 1 n))

;; Optimized recursive process (O(log n))
(define (exponent-opti x n)
  (cond ((= n 0) 1)
        ((= n 2) (* x x)) ;; So even? branch doesnt infinitely loop - alternatively use (square)
        ((even? n)
         (exponent-opti (exponent-opti x (/ n 2)) 2))
        (else
         (* x (exponent-opti x (- n 1))))))

;; Redefinitions of some in built procedures for fun
(define (length-rec lst)
  (if (null? lst)
      0
      (+ 1 (length-rec (cdr lst)))))

(define (length-iter lst)
  (define (length-iter-impl acc rest)
    (if (null? rest)
        acc
        (length-iter-impl (+ 1 acc)
                          (cdr rest))))
  (length-iter-impl 0 lst))

;; Doing map manually
(define (sqr-all-rec lst)
  (if (null? lst)
    '()
    (let ((cur (car lst)))
      (cons (* cur cur)
            (sqr-all-rec (cdr lst))))))

(define (sqr-all-iter lst)
  (define (sqr-all-iter-impl rest acc)
    (if (null? rest)
      (reverse acc)
      (let ((cur (car rest)))
        (sqr-all-iter-impl (cdr rest)
                           (cons (* cur cur)
                                 acc)))))
  (sqr-all-iter-impl lst '()))

(define (sqr-all-destructive lst)
  (if (not (null? lst))
      (begin
        (set-car! lst (* (car lst) (car lst)))
        (sqr-all-destructive (cdr lst)))))

(define (abs-all lst)
  (if (null? lst)
    '()
    (cons (abs (car lst))
          (abs-all (cdr lst)))))

;; Higher order procedures
(define (my-map proc lst)
  (if (null? lst)
    '()
    (cons (proc (car lst))
          (my-map proc (cdr lst)))))

(define (my-map-iter proc lst)
  (define (my-map-iter-impl rest acc)
    (if (null? rest)
      (reverse acc) ;; bad
      (my-map-iter-impl (cdr rest)
                        (cons (proc (car rest))
                              acc))))
  (my-map-iter-impl lst '()))
