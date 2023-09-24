;;;;
;;;; Prekode til innlevering 2a i IN2040 (H23): Prosedyrer for å jobbe med
;;;; Huffman-trær, fra SICP, Seksjon 2.3.4.
;;;;

;;; Merk at koden under gjør bruk av diverse innebygde kortformer for
;;; kjeder av car og cdr. F.eks er (cadr x) det samme som (car (cdr x)), 
;;; og (caadr x) tilsvarer (car (car (cdr x))), osv. 



;;;
;;; Abstraksjonsbarriere for trær:
;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;;;
;;; Dekoding:
;;;

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (if (= bit 0) 
      (left-branch branch)
      (right-branch branch)))


;;;
;;; Sortering av node-lister:
;;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;;; Eksempel på kodebok:
(define sample-tree
  (make-code-tree
    (make-code-tree
      (make-leaf 'fight 6)
      (make-leaf 'ninjas 5))
    (make-code-tree
      (make-leaf 'samurais 4)
      (make-code-tree
        (make-leaf 'night 2)
        (make-leaf 'by 1)))))

;;; Eksempelkode: 
(define sample-code '(1 0 0 0 0 1 1 1 1 1 1 0))
