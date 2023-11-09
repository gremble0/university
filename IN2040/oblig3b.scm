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

;; Oppgave 2
;;; a:
;;;; Endret prosedyre
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        (list '1+ ;; ENDRING: oppgave 2a
              (lambda (x) (+ x 1)))
        (list '1- ;; ENDRING: oppgave 2a
              (lambda (x) (- x 1)))
        ))

;; for kopiering:
(set! the-global-environment (setup-environment))
(the-global-environment)
