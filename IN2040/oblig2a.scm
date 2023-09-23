;; Oppgave 1
;;; a:
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;;; b:
(define foo 42)

((lambda (foo x)
   (if (= x foo)
       'same
       'different))
 5 foo) ;; => different

((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel) ;; => (towel (42 towel))
