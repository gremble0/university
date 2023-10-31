;; Importer prekode
(load "prekode3a.scm")

;; Oppgave 1
;;; a, b:
;;;; Prosedyren klarer å memoisere prosedyrer, men 'unmemoize grenen
;;;; ser ikke ut til å virke.
(define (mem message proc)
  (let ((results (make-table)))
    (lambda args
      (cond ((eq? 'unmemoize message)
             (apply proc args))
            ((eq? 'memoize message)
             (let ((memoized (lookup args results)))
               (cond ((not memoized)
                      (let ((result (apply proc args)))
                        (insert! args result results)
                        result))
                     (else memoized))))))))


(set! fib (mem 'memoize fib))
(fib 3) ;; ~>
;; computing fib of 3
;; computing fib of 2
;; computing fib of 1
;; computing fib of 0
;; -> 2
(fib 3) ;; -> 2
(fib 2) ;; -> 1
(fib 4) ;; ~>
;; computing fib of 4
;; -> 3
(set! fib (mem 'unmemoize fib)) ;; virker ikke som i oppgaveteksten
(fib 3)
;; -> 3

(set! test-proc (mem 'memoize test-proc))
(test-proc) ;; ~>
;; computing test-proc of ()
;; -> 0
(test-proc) ;; -> 0
(test-proc 40 41 42 43 44) ;; ~>
;; computing test-proc of (40 41 42 43 44)
;; computing test-proc of (40 41 42 43)
;; computing test-proc of (40 41 42)
;; computing test-proc of (40 41)
;; computing test-proc of (40)
;; computing test-proc of (40)
;; -> 10
(test-proc 40 41 42 43 44) ;; -> 10
(test-proc 42 43 44) ;; -> 5


;;; c:
;;;; Siden vi i tilordner mem-fib til (mem 'memoize fib) vil cachingen kun lagres for
;;;; prosedyrekallene til mem-fib, og ikke for de rekursive kallene innad i fib som kaller
;;;; på prosedyren (fib). Derfor husker mem-fib bare resultatene for sine egne prosedyrekall
;;;; og ikke resultatene av sin egen rekursjon.

;; Oppgave 2
;;; a:
(define (list-to-stream lst)

  (define (list-to-stream-impl lst stream-acc)
    (if (null? lst)
        stream-acc
        (list-to-stream-impl (cdr lst) (cons-stream (car lst) stream-acc))))

  (list-to-stream-impl (reverse lst) the-empty-stream))


(list-to-stream '(1 2 3 4 5)) ;; -> (1 . #<promise>)


(define (stream-to-list stream . len)

  (define (stream-to-list-full-impl stream lst-acc)
    (if (stream-null? stream)
        (reverse lst-acc)
        (stream-to-list-full-impl
         (stream-cdr stream)
         (cons (stream-car stream) lst-acc))))

  (define (stream-to-list-n-impl stream lst-acc len)
    (if (stream-null? stream)
        (reverse lst-acc) ;; returner hele listen hvis len er større enn listens lengde
        (if (= len 0)
            (reverse lst-acc)
            (stream-to-list-n-impl
             (stream-cdr stream)
             (cons (stream-car stream) lst-acc)
             (- len 1)))))

  (if (null? len)
      (stream-to-list-full-impl stream '())
      (stream-to-list-n-impl stream '() (car len))))


(stream-to-list (stream-interval 10 20)) ;; -> (10 11 12 13 14 15 16 17 18 19 20)
(show-stream nats 15) ;; -> 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
(stream-to-list nats 10) ;; -> (1 2 3 4 5 6 7 8 9 10)

;;; b:
;; (define (stream-reverse stream)
(define (stream-take n stream)
  
  (define (stream-take-impl n stream lst-acc)
    (if (stream-null? stream)
        lst-acc ;; samme som forrige oppgave i tilfelle prosedyre brukes på endelig strøm
        (if (= n 0)
            lst-acc
            (stream-take-impl
             (- n 1)
             (stream-cdr stream)
             (cons (stream-car stream) lst-acc)))))

  ;; litt krunglete, men det virker
  (list-to-stream (reverse (stream-take-impl n stream the-empty-stream))))


(define foo (stream-take 10 nats))
foo ;; -> (1 . #<promise>)
(show-stream foo 5) ;; ~> 1 2 3 4 5 ...
(show-stream foo 20) ;; ~> 1 2 3 4 5 6 7 8 9 10
(show-stream (stream-take 15 nats) 10) ;; ~> 1 2 3 4 5 6 7 8 9 10 ...

;;; c:
;;;; Å bytte ut alle listeoperasjoner for memq og remove-duplicates med strømoperasjoner
;;;; virker som forventet på endelige strømmer. Problemet oppstår hvis man prøver å
;;;; å bruke den på en udendelig strøm. Da vil den gå i en uendelig løkke fordi predikatet
;;;; `stream-null?' alltid vil returnere #f.

;;; d:
(define (stream-memq item x)
  (cond ((stream-null? x) #f)
        ((eq? item (stream-car x)) x)
        (else (stream-memq item (stream-cdr x)))))

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      ;; trenger ikke kjøre cons-stream
      (stream-filter (lambda (item)
                       (set! stream (stream-cdr stream))
                       (not (stream-memq item (stream-cdr stream))))
                     (stream-cdr stream))))
