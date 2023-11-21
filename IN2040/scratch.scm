;; Recursive process
(define (fac-rec n)
  (if (= n 1)
      1
      (* n (fac-rec (- n 1)))))

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

;; Iterative process
(define (fac-iter n)
  (define (fac-iter-impl n acc)
    (if (= n 0)
        acc
        (fac-iter-impl (- n 1) (* n acc))))
  (fac-iter-impl n 1))
