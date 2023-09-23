;; Oppgave 1
;;; a:
(define (p-cons x y)
  (lambda (l) (l x y)))

(define (p-car l)
  (l (lambda (x y) x)))

(define (p-cdr l)
  (l (lambda (x y) y)))
