;;; "Metacircular evaluator", basert på koden i seksjon 4.1.1-4.1.4 i SICP.
;;; Del av innlevering 3b i IN2040, høst 2020, 2021, 2022, 2023
;; 
;; Last hele filen inn i Scheme. For å starte read-eval-print loopen og 
;; initialisere den globale omgivelsen, kjør:
;; (set! the-global-environment (setup-environment))
;; (read-eval-print-loop)
;;
;; Merk at det visse steder i koden, som i `special-form?', vanligvis ville
;; være mere naturlig å bruke `or' enn `cond'. Evaluatoren er skrevet helt
;; uten bruk av `and' / `or' for å vise at disse likevel kan støttes i det
;; implementerte språket selv om de ikke brukes i
;; implementeringsspråket. (Se oppgave 3a for mer om dette.)

;; hack for å etterlikne SICPs feilmeldinger:
(define exit-to-toplevel 'dummy)
(call-with-current-continuation 
 (lambda (cont) (set! exit-to-toplevel cont)))

(define (error reason . args)
  (display "ERROR: ")
  (display reason)
  (for-each (lambda (arg) 
	      (display " ")
	      (write arg))
	    args)
  (newline)
  (exit-to-toplevel))


;;; Selve kjernen i evaluatoren (seksjon 4.1.1, SICP):
;;; -----------------------------------------------------------------------

;; Merk at vi skiller ut evaluering av special forms i en egen prosedyre.

(define (mc-eval exp env) ;; tilsvarer eval i SICP
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((special-form? exp) (eval-special-form exp env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- mc-eval:" exp))))

(define (mc-apply proc args) ;; tilsvarer apply i SICP
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence
          (procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- mc-apply:" proc))))

;; UENDRET VERSJON FRA PREKODE
;; (define (eval-special-form exp env)
;;   (cond ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp) 
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (mc-eval (cond->if exp) env))))
;;
;; (define (special-form? exp)
;;   (cond ((quoted? exp) #t)
;;         ((assignment? exp) #t)
;;         ((definition? exp) #t)
;;         ((if? exp) #t)
;;         ((lambda? exp) #t)
;;         ((begin? exp) #t)
;;         ((cond? exp) #t)
;;         (else #f)))

;; START ENDRINGER OPPGAVE 3a, 3c, 3d, 4:
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
        ((and? exp) (eval-and (and-exprs exp) env)) ;; ENDRING 3a
        ((or? exp) (eval-or (or-exprs exp) env)) ;; ENDRING 3a
        ((let? exp) (eval-let-special exp env)) ;; ENDRING 3c, 3d (endre til eval-let for 3c)
        ((while? exp) (eval-while exp env)))) ;; ENDRING 4

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; ENDRING 3a
        ((or? exp) #t) ;; ENDRING 3a
        ((let? exp) #t) ;; ENDRING 3c, 3d
        ((while? exp) #t) ;; ENDRING 4
        (else #f)))
;; SLUTT ENDRINGER OPPGAVE 3a, 3c, 3d, 4

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; UENDRET VERSJON FRA PREKODE:
;; (define (eval-if exp env)
;;   (if (true? (mc-eval (if-predicate exp) env))
;;       (mc-eval (if-consequent exp) env)
;;       (mc-eval (if-alternative exp) env)))

;; START ENDRINGER OPPGAVE 3b:
(define (eval-if exp env)
  (if (true? (mc-eval (if/elsif-predicate exp) env))
      (if (else-exists? exp)
          (mc-eval (if/elsif-consequent exp) env))
      (eval-elsifs (next-else-or-elsif exp) env)))

(define (eval-elsifs exp env)
  (if (else? exp)
      (mc-eval (else-consequent exp) env)
      (if (true? (mc-eval (if/elsif-predicate exp) env))
          (if (else-exists? exp)
              (mc-eval (if/elsif-consequent exp) env))
          (eval-elsifs (next-elsif exp) env))))
;; SLUTT ENDRINGER OPPGAVE 3b

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

;; START ENDRINGER OPPGAVE 3a, 3c, 3d, 4
;;; 3a:
(define (eval-and exp env)
  (cond ((null? exp) #t)
        ((false? (mc-eval (and-first-exp exp) env)) #f)
        (else (eval-and (and-rest-exp exp) env))))

(define (eval-or exp env)
  (cond ((null? exp) #f)
        ((false? (mc-eval (or-first-exp exp) env))
         (eval-or (or-rest-exp exp) env))
        (else #t)))

;;; 3c:
(define (eval-let exp env)
  (let ((proc (make-lambda (let-vars exp) (let-body exp))))
    (mc-eval (cons proc (let-vals exp)) env)))

;;; 3d:
(define (eval-let-special exp env)
  (let* ((bindings (let-special-bindings (cdr exp)))
         (vars (map car bindings))
         (vals (map cdr bindings))
         (proc (make-lambda vars (let-special-body (cdr exp)))))
    (mc-eval (cons proc vals) env)))

;;; 4:
;;;; 
(define (eval-while exp env)
  (define (eval-iter exp)
    (mc-eval exp env))
    
  (if (mc-eval (while-cond exp) env)
      (begin
        (map eval-iter (while-body exp))
        (eval-while exp env))
      'ok))
;; SLUTT ENDRINGER OPPGAVE 3a, 3c, 3d, 4

;;; Predikater + selektorer som definerer syntaksen til uttrykk i språket 
;;; (seksjon 4.1.2, SICP)
;;; -----------------------------------------------------------------------

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((boolean? exp) #t)
        (else #f)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))


(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; START ENDRINGER OPPGAVE 3a, 3c, 3d, 4:
;; 3a:
(define (and? exp) (tagged-list? exp 'and))

(define (and-exprs exp) (cdr exp))
(define (and-first-exp exp) (car exp))
(define (and-rest-exp exp) (cdr exp))

(define (or? exp) (tagged-list? exp 'or))

(define (or-exprs exp) (cdr exp))
(define (or-first-exp exp) (car exp))
(define (or-rest-exp exp) (cdr exp))

;; 3c:
(define (let? exp) (tagged-list? exp 'let))

;; Litt ueffektivt å mappe over samme exp to ganger, men mye simplere sånn
;; (kunne alternativt lagd en prosedyre som returnerer et par av de to)
(define (let-vars exp) (map car (cadr exp)))
(define (let-vals exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

;; 3d:
(define (let-special-and? exp)
  (eq? (cadddr exp) 'and))
(define (let-special-final-assignment? exp)
  (eq? (cadddr exp) 'in))

(define (let-special-var exp) (car exp))
(define (let-special-val exp)
  (if (eq? (cadr exp) '=)
      (caddr exp)
      (error "expected = token" exp)))

(define (let-special-next-assignment exp)
  (if (let-special-and? exp)
      (cddddr exp)
      (error "expected and or in token" exp)))

(define (let-special-body exp)
  (if (let-special-final-assignment? exp)
      (cddddr exp)
      (let-special-body (let-special-next-assignment exp))))

(define (let-special-bindings exp)
  (let ((var (let-special-var exp))
        (val (let-special-val exp)))
    (if (let-special-final-assignment? exp)
        (cons (cons var val) '())
        (cons (cons var val) (let-special-bindings (let-special-next-assignment exp))))))

;; 4:
(define (while? exp) (tagged-list? exp 'while))
(define (while-cond exp) (cadr exp))
(define (while-body exp) (cddr exp))

;; SLUTT ENDRINGER 3a, 3c, 3d, 4

;; UENDRET VERSJON FRA PREKODE
;; (define (if? exp) (tagged-list? exp 'if))
;;
;; (define (if-predicate exp) (cadr exp))
;;
;; (define (if-consequent exp) (caddr exp))
;;
;; (define (if-alternative exp)
;;   (if (not (null? (cdddr exp)))
;;       (cadddr exp)
;;       'false))
;;
;; (define (make-if predicate consequent alternative)
;;   (list 'if predicate consequent alternative))

;; START ENDRINGER OPPGAVE 3b:
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
;; SLUTT ENDRINGER OPPGAVE 3b

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF:"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;;; Evaluatorens interne datastrukturer for å representere omgivelser,
;;; prosedyrer, osv (seksjon 4.1.3, SICP):
;;; -----------------------------------------------------------------------

(define (false? x)
  (cond ((eq? x 'false) #t)
        ((eq? x #f) #t)
        (else #f)))

(define (true? x)
  (not (false? x)))
;; (som i SICP-Scheme'en vi tar med true/false som boolske verdier.)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define the-empty-environment '())

;; En ramme er et par der car er variablene
;; og cdr er verdiene:
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied:" vars vals)
          (error "Too few arguments supplied:" vars vals))))

;; Søker gjennom listene av variabel-bindinger i første ramme og 
;; så bakover i den omsluttende omgivelsen. (Moro; to nivåer av 
;; interne definisjoner med gjensidig rekursjon.) 
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals) 
      ; parallell rekursjon på listene av symboler og verdier
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable:" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; Endrer bindingen av 'var' til 'val' i en omgivelse 
;; (gir feil dersom 'var' ikke er bundet):
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!:" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; define-variable! legger til en ny binding mellom 'var' og 'val' 
;; i den første rammen i omgivelsen 'env':
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;; Håndtering av primitiver og den globale omgivelsen (SICP seksjon 4.1.4)
;;; -----------------------------------------------------------------------

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true 'true initial-env)
    (define-variable! 'false 'false initial-env)
    (define-variable! 'nil '() initial-env)
    initial-env))

(define the-global-environment the-empty-environment)
;; For initialisering av den globale omgivelsen, se kommentar til slutt i fila.

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; UENDRET VERSJON FRA PREKODE:
;; (define primitive-procedures
;;   (list (list 'car car)
;;         (list 'cdr cdr)
;;         (list 'cons cons)
;;         (list 'null? null?)
;;         (list 'not not)
;;         (list '+ +)
;;         (list '- -)
;;         (list '* *)
;;         (list '/ /)
;;         (list '= =)
;;         (list 'eq? eq?)
;;         (list 'equal? equal?)
;;         (list 'display 
;;               (lambda (x) (display x) 'ok))
;;         (list 'newline 
;;               (lambda () (newline) 'ok))
;; ;;      her kan vi legge til flere primitiver.
;;         ))

;; START ENDRINGER OPPGAVE 2a:
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
        (list '1+ ;; ENDRING
              (lambda (x) (+ x 1)))
        (list '1- ;; ENDRING
              (lambda (x) (- x 1)))
        ))
;; SLUTT ENDRINGER OPPGAVE 2a

;; START ENDRINGER OPPGAVE 2b:
(define (install-primitive! name proc)
  (let ((updated-vars (cons name (caar the-global-environment)))
        (updated-vals (cons (list 'primitive proc) (cdar the-global-environment))))
    (set! the-global-environment (list (cons updated-vars updated-vals)))))
;; SLUTT ENDRINGER OPPGAVE 2b

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


;;; Hjelpeprosedyrer for REPL-interaksjon (SICP seksjon 4.1.4)
;;; -----------------------------------------------------------------------

(define input-prompt ";;; MC-Eval input:")
(define output-prompt ";;; MC-Eval value:")

(define (read-eval-print-loop) ;;tilsvarer driver-loop i SICP
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (read-eval-print-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

'METACIRCULAR-EVALUATOR-LOADED

;;; For å starte read-eval-print loopen og initialisere 
;;; den globale omgivelsen, kjør:
;;; (set! the-global-environment (setup-environment))
;;; (read-eval-print-loop)
