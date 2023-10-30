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

;;;; Test fib
(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)
(set! fib (mem 'unmemoize fib))
(fib 3)

;;;; Test test-proc
(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)

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

;;;; test list-to-stream
(list-to-stream '(1 2 3 4 5)) ;; -> (1 . #<promise>)

(define (stream-to-list lst . len)
  (define (list-to-stream-impl lst lst-acc . len)
    )
  (if (null? len)
      (list-to-stream-impl lst '())
      (list-to-stream-impl lst '() (car len))))
