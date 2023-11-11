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

;;; b:
;;;; Ny prosedyre
(define (install-primitive! name proc)
  (let ((updated-vars (cons name (caar the-global-environment)))
        (updated-vals (cons (list 'primitive proc) (cdar the-global-environment))))
    (set! the-global-environment (list (cons updated-vars updated-vals)))))

;; Oppgave 3
;;; a:
;;;; Alle nye og endrede prosedyrer:
(define (and? exp) (tagged-list? exp 'and))

(define (and-exprs exp) (cdr exp))
(define (and-first-exp exp) (car exp))
(define (and-rest-exp exp) (cdr exp))

(define (or? exp) (tagged-list? exp 'or))

(define (or-exprs exp) (cdr exp))
(define (or-first-exp exp) (car exp))
(define (or-rest-exp exp) (cdr exp))

(define (eval-and exp env)
  (cond ((null? exp) #t)
        ((false? (mc-eval (and-first-exp exp) env)) #f)
        (else (eval-and (and-rest-exp exp) env))))

(define (eval-or exp env)
  (cond ((null? exp) #f)
        ((false? (mc-eval (or-first-exp exp) env))
         (eval-or (or-rest-exp exp) env))
        (else #t)))

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and (and-exprs exp) env)) ;; ENDRING
        ((or? exp) (eval-or (or-exprs exp) env)))) ;; ENDRING

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; ENDRING
        ((or? exp) #t) ;; ENDRING
        (else #f)))

;;; b:
;;;; Alle nye og endrede prosedyrer:
(define (if? exp) (tagged-list? exp 'if))
(define (elsif? exp) (tagged-list? exp 'elsif))
(define (else? exp) (tagged-list? exp 'else))

(define (next-else-or-elsif exp) (cddddr exp))

(define (next-elsif exp)
  (if (elsif? exp)
      (cddddr exp)
      (error "expected elsif token" exp)))

(define (if/elsif-predicate exp) (cadr exp))

(define (if/elsif-consequent exp)
  (if (eq? (caddr exp) 'then)
      (cadddr exp)
      (error "expected then token" exp)))

(define (else-consequent exp) (cadr exp))

;; Underliggende scheme gjør ikke dette, men oppgaveteksten sier else skal
;; være obligatorisk
(define (else-exists? exp)
  (cond ((null? exp) (error "expected expression to end with else"))
        ((else? exp) #t)
        (else (else-exists? (next-else-or-elsif exp)))))

(define (eval-if exp env)
  (if (true? (mc-eval (if/elsif-predicate exp) env))
      (if (else-exists? exp)
          (mc-eval (if/elsif-consequent exp) env))
          ;; (error "expected expression to end with else"))
      (eval-elsifs (next-else-or-elsif exp) env)))

(define (eval-elsifs exp env)
  (if (else? exp)
      (mc-eval (else-consequent exp) env)
      (if (true? (mc-eval (if/elsif-predicate exp) env))
          (if (else-exists? exp)
              (mc-eval (if/elsif-consequent exp) env))
              ;; (error "expected expression to end with else"))
          (eval-elsifs (next-elsif exp) env))))



;; for kopiering:
(set! the-global-environment (setup-environment))
(read-eval-print-loop)
