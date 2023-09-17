;; Oppgave 1
;;; f:
(define f '(0 42 #t bar))
(car (cdr f)) ;; -> 42

;;; g:
(define g '((0 42) (#t bar)))
(car (cdr (car g))) ;; -> 42

;;; h:
(define h '((0) (42 #t) (bar)))
(car (car (cdr h))) ;; -> 42

;;; i:
(define i-cons
  (cons (cons 0 42)
        (cons (cons #t 'bar) '())))

(define i-list
  (list '(0 42)
        '(#t bar)))

;; Oppgave 2
;;; a:
(define (take n items)
  (define (impl n items acc)
    (if (or (equal? n 0)
            (empty? items))
        (reverse acc)
        (impl (- n 1) (cdr items) (cons (car items) acc))))
  (impl n items '()))
