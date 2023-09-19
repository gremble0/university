;; Oppgave 1
;;; f:
(define f '(0 42 #t bar))
(car (cdr f)) ;; -> 42

;;; g:
(define g '((0 42) (#t bar)))
(car (cdr (car g))) ;; -> 42

;;; h:
(define h '((0) (42 #t) (bar)))
(car (car (cdr h))) ;; -> 42

;;; i:
(define i-cons
  (cons (cons 0 42)
        (cons (cons #t 'bar) '())))

(define i-list
  (list '(0 42)
        '(#t bar)))

;; Oppgave 2
;;; a:
(define (take-rec n items)
  (if (or (equal? n 0)
          (empty? items))
      '()
      (cons (car items) (take-rec (- n 1) (cdr items)))))

;;; b:
;;;; Vi kan ikke fjerne noen av argumentene til den indre prosedyren siden de alle blir modifisert
;;;; i de rekursive prosedyrekallene (av prosedyrekallene (-) (cdr) og (cons))
(define (take-iter n items)
  (define (take-iter-impl n rest acc)
    (if (or (equal? n 0)
            (empty? rest))
        (reverse acc)
        (take-iter-impl (- n 1) (cdr rest) (cons (car rest) acc))))
  (take-iter-impl n items '()))

;;; c:
(define (take-while pred items)
  (define (take-while-impl rest acc)
    (if (pred (car rest))
        (take-while-impl (cdr rest) (cons (car rest) acc))
        (reverse acc)))
  (take-while-impl items '()))


;;; d:
(define (map2 proc list1 list2)
  (define (map2-impl rest1 rest2 acc)
    (if (or (empty? rest1)
            (empty? rest2))
        (reverse acc)
        (map2-impl (cdr rest1) (cdr rest2) (cons (proc (car rest1) (car rest2)) acc))))
  (map2-impl list1 list2 '()))


;;; e:
(map2 (lambda (x y)
        (/ (+ x y) 2))
      '(1 2 3 4) '(3 4 5)) ;; -> '(2 3 4)
