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

(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook) ;; => (a b c)
