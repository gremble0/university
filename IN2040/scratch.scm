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
        (fac-iter-impl (- n 1) (* n acc))))
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
(define (exponent x n)
    (define (exponent-impl acc n)
      (if (= n 0)
          acc
          (exponent-impl (* x acc)
                         (- n 1))))
    (exponent-impl 1 n))

;; Optimized recursive process (O(log n))
(define (exponent x n)
      (cond ((= n 0) 1)
            ((= n 2) (* x x)) ;; So even? branch doesnt infinitely loop - alternatively use (square)
            ((even? n)
             (exponent (exponent x (/ n 2)) 2))
            (else
             (* x (exponent x (- n 1))))))
