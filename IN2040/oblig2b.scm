;; Oppgave 1
;;; a:
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))
(c1) ;; -> 1
(c1) ;; -> 2
(c1) ;; -> 3
count ;; -> 42
(c2) ;; -> 1


;; Oppgave 2
;;; a:
(define (make-stack stack)
  (define (push! . elements)
    (cond ((null? elements) stack)
          (else (set! stack (cons (car elements) stack))
                (push! (cdr elements)))))
  (define (pop!)
    (cond ((null? stack) stack)
          (else (set! stack (cdr stack)))))
  (define (dispatch . args)
    (let ((message (car args)))
          (cond ((eq? message 'push!) (push! (cdr args)))
                ((eq? message 'pop!)  (pop!))
                ((eq? message 'stack) stack))))
  dispatch)

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)
