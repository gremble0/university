;; Importer prekode
(load "prekode3a.scm")

;; Oppgave 1
;;; a:

(define (mem message proc)
  (let ((results (make-table)))
    (if (eq? message 'memoize)
        (lambda args
          (let ((memoized (lookup args results)))
            (cond ((not memoized)
                   (let ((result (apply proc args)))
                     (insert! args result results)
                     result))
                  (else memoized))))
        proc)))

(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)
