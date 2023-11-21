;; Oppgave 1
;;; a:
;;;; Når `mc-eval' kalles på prosedyren `foo' ignoreres de forekomstene av
;;;; nøkkelordene `cond' og `else' som er i forventet posisjon for en cond blokk.
;;;; derfor vil disse behandles annerledes enn andre forekomster av symbolene
;;;; cond og else som vil bruke bindingene lokale for prosedyren foo.
;;;;
;;;; (foo 2 square) -> 0
;;;; cond er bundet til 2, som betyr at (= cond 2) -> #t så vi returnerer 0
;;;;
;;;; (foo 4 square) -> 16
;;;; cond er bundet til noe som ikke er 2 så (= cond 2) -> #f, så vi returnerer
;;;; resultatet av å kalle prosedyren bundet til else (square) med argumentet cond (4).
;;;; (square 4) -> 16. Så vi returnerer 16.
;;;;
;;;; (cond ((= cond 2) 0)
;;;;       (else (else 4))) -> 2
;;;; cond bruker den globale bindingen til 3 som betyr at (= cond 2) -> #f. Så vi
;;;; returnerer resultatet av å kalle prosedyren bundet til else med argumentet 4.
;;;; else bruker den globale bindingen til prosedyren (lambda (x) (/ x 2)).
;;;; ((lambda (x) (/ x 2)) 4) -> 2. Så vi returnerer 2.

;; Oppgave 4
;;; Syntaks for while løkke:
;;; (while <predicate>
;;;   <exp1>
;;;   <exp2>
;;;    ...
;;;   <expn>)
;;;
;;; for eksempel:
(define x 5)

(while (not (= x 0))
       (display x) (newline)
       (set! x (- x 1)))
;;~> 5
;;   4
;;   3
;;   2
;;   1
;;   'ok

;;; while som destruktiv filter:
(define lst '(1 #f foo () -1))
(define truthy-values '())

(while (not (eq? lst '()))
       (if (car lst) then
           (set! truthy-values (cons (car lst) truthy-values))
           else (display "falsy value"))
       (set! lst (cdr lst)))
;; ~> falsy value
;;    'ok
truthy-values
;; -> (-1 () foo 1)


;; for kopiering:
(set! the-global-environment (setup-environment))
(read-eval-print-loop)
