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
(stack s1) ;; -> (faa foo zip zap bah bar)

;; Oppgave 3
;;; a:
;;;; Siden prosedyrekallet set-cdr! gjør listen bar til en sirkulær liste vil å
;;;; kalle cdr på listen etter hvert gjøre at man ender opp på starten av listen igjen.
;;;; Derfor starter man på b igjen etter å gjøre car av 4xcdr (list-ref bar 4), så
;;;; teller man opp igjen derfra frem til man går tilbake til starten igjen.
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
(list-ref bar 0) ;; -> a
(list-ref bar 3) ;; -> d
(list-ref bar 4) ;; -> b
(list-ref bar 5) ;; -> c

;;; c:
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)

(define (cycle? lst)
  (define (cycle?-impl slow fast)
    (cond ((null? fast) #f)
          ((null? (cdr fast)) #f)
          ((eq? slow fast) #t)
          (else (cycle?-impl (cdr slow) (cddr fast)))))
  (cycle?-impl lst (cdr lst)))

(cycle? '(hey ho))
(cycle? '(la la la))
(cycle? bah)
(cycle? bar)

;;; d:
;;;; Lister i scheme er definert som en kjede med par som avsluttes med den tomme
;;;; listen '(). Siden sirkulære lister aldri avsluttes med '() er de derfor strengt
;;;; tatt ikke lister. Siden bar er sirkulær er den derfor ikke en list?, mens siden
;;;; bah avsluttes av en '() stemmer det at den er en list?.
