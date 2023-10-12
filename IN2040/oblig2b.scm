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
;;;; Jeg la også til returverdier for pop! og push! så man kan se tilstanden til
;;;; stack uten å eksplisitt hente den med 'stack
(define (make-stack stack)
  (define (push! x)
    (cond ((null? x) stack)
          (else (set! stack (cons (car x) stack))
                      (push! (cdr x)))))
  (define (pop!)
    (cond ((null? stack) stack)
          (else (set! stack (cdr stack)) stack)))
  (define (dispatch . args)
    (let ((message (car args)))
          (cond ((eq? message 'push!) (push! (cdr args)))
                ((eq? message 'pop!)  (pop!))
                ((eq? message 'stack) stack))))
  dispatch)

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!) ;; -> (bar)
(s1 'stack) ;; -> (bar)
(s2 'pop!) ;; -> ()
(s2 'push! 1 2 3 4) ;; -> (4 3 2 1)
(s2 'stack) ;; -> (4 3 2 1)
(s1 'push! 'bah) ;; (bah bar)
(s1 'push! 'zap 'zip 'baz) ;; -> (baz zip zap bah bar)
(s1 'stack) ;; -> (baz zip zap bah bar)

;;; b:
(define (pop! stack)
  (stack 'pop!))

(define (push! stack . x)
  (apply stack 'push! x))

(define (stack s)
  (s 'stack))
  
(pop! s1) ;; -> (zip zap bah bar)
(stack s1) ;; -> (zip zap bah bar)
(push! s1 'foo 'faa) ;; -> (faa foo zip zap bah bar)
(stack s1) ;; ;; -> (faa foo zip zap bah bar)
