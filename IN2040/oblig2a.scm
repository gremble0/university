;; Importer prekode (må evalueres før kode fra oppgave 2)
(load "huffman.scm")

;; Oppgave 1
;;; a:
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;;; b:
(define foo 42)

((lambda (foo x)
   (if (= x foo)
       'same
       'different))
 5 foo) ;; => different

((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel) ;; => (towel (42 towel))

;;; c:
;;;; Prosedyrekallet nedenfor fører til en error siden prosedyren forventer at det midterste
;;;; elementet i listen skal være en prosedyre og ikke et uevaluert symbol som '/
(define (infix-eval l)
  ((cadr l) (car l) (caddr l)))

(define bah '(84 / 2))
(infix-eval bah) ;; => error

;; Oppgave 2
;;; a:
(define (decode bits tree)
  (define (decode-impl bits current-branch out)
    (if (null? bits)
        (reverse out)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-impl (cdr bits) tree (cons (symbol-leaf next-branch) out))
              (decode-impl (cdr bits) next-branch out)))))
  (decode-impl bits tree '()))

;;; b:
(decode sample-code sample-tree) ;; => (samurais fight ninjas by night)

;;; c:
(define (encode message tree)
  (define (encode-impl message current-branch out)
    (if (null? message)
        (reverse out)
        (cond ((leaf? current-branch)
               (encode-impl (cdr message) tree out))
              ((memq (car message) (symbols (right-branch current-branch))) ;; memq sjekker om et element er i en liste
               (encode-impl message (right-branch current-branch) (cons 1 out)))
              (else ;; Antar at `message' aldri inneholder symboler ikke definert i `tree'
               (encode-impl message (left-branch current-branch) (cons 0 out))))))
  (encode-impl message tree '()))

(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree) ;; => (ninjas fight ninjas)

;;; d:
(define (grow-huffman-tree freqs)
  (define (grow-huffman-tree-impl freqs)
    (if (null? (cdr freqs))
        (car freqs)
        (grow-huffman-tree-impl (adjoin-set (make-code-tree (car freqs) (cadr freqs)) (cddr freqs)))))
  (grow-huffman-tree-impl (make-leaf-set freqs)))

(define freqs-2d '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook-2d (grow-huffman-tree freqs-2d))
(decode (encode '(a b c) codebook) codebook) ;; => (a b c)

;;; e:
;;;; - Vi ser fra returverdien til (length encoded-2e) at meldingen tar 43 bits å kode.
;;;;
;;;; - For å finne gjennomsnittlig antall bits brukt per kodeord i meldingen kan vi ta
;;;;   antall bits for hele meldingen delt på antall symboler i meldingen. Da får vi:
;;;;   (/ (length encoded-2e) (length freqs-2e)) = 43/16
;;;;   Vi kan så runde dette opp med (round) for å få 3
;;;; 
;;;; - Siden alle symboler representers av et likt antall bits i fixed-length code trenger
;;;;   vi bare finne ut hvor mange bits hvert symbol krever for vårt alfabet. For å gjøre
;;;;   dette må vi runde opp lengden av alfabetet vårt til den nærmeste eksponenten av 2.
;;;;   (det er 2 muligheter per bit) Siden (length freqs-2e) = 16 og 2^4 = 16 ser vi at
;;;;   hvert symbol krever 4 bits. Nå gjenstår det bare å multiplisere kodeordlengden 4 med
;;;;   antall symboler som gir oss (* 4 (length message-2e)) = 68
(define freqs-2e '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3)
                   (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by 12)
                   (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))
(define codebook-2e (grow-huffman-tree freqs-2e))

(define message-2e '(ninjas fight
                     ninjas fight ninjas
                     ninjas fight samurais
                     samurais fight
                     samurais fight ninjas
                     ninjas fight by night))
(define encoded-2e (encode message-2e codebook-2e))

;;;; e1
(length encoded-2e) ;; => 43

;;;; e2
(round (/ (length encoded-2e) (length freqs-2e))) ;; => 3

;;;; e3
(* 4 (length message-2e)) ;; => 68

;;; f:
(define (huffman-leaves tree)
  (define (huffman-leaves-impl current-branch out)
    (if (leaf? tree)
        (cons (list (symbol-leaf tree) (weight-leaf tree)) out)
        (append (huffman-leaves (left-branch tree)) (huffman-leaves (right-branch tree)))))
  (huffman-leaves-impl tree '()))

(huffman-leaves sample-tree) ;; => ((fight 6) (ninjas 5) (samurais 4) (night 2) (by 1))
