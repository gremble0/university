;; Recursive process
(define (fac-rec n)
  (if (= n 1)
      1
      (* n (fac-rec (- n 1)))))

;; Iterative process
(define (fac-iter n)
  (define (fac-iter-impl n acc)
    (if (= n 0)
        acc
        (fac-iter-impl (- n 1)
                       (* n acc))))
  (fac-iter-impl n 1))

;; Three-recursive process
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

;; Iterative process
(define (fib-iter n)
  (define (fib-iter-impl next cur count)
    (if (= count n)
        cur
        (fib-iter-impl (+ next cur)
                       next
                       (+ count 1))))
  (fib-iter-impl 1 0 0))

;; Recursive process
(define (exponent x n)
  (if (= n 0)
      1
      (* x (exponent x (- n 1)))))

;; Iterative process
(define (exponent-iter x n)
  (define (exponent-iter-impl acc n)
    (if (= n 0)
        acc
        (exponent-iter-impl (* x acc)
                       (- n 1))))
  (exponent-iter-impl 1 n))

;; Optimized recursive process (O(log n))
(define (exponent-opti x n)
  (cond ((= n 0) 1)
        ((= n 2) (* x x)) ;; So even? branch doesnt infinitely loop - alternatively use (square)
        ((even? n)
         (exponent-opti (exponent-opti x (/ n 2)) 2))
        (else
         (* x (exponent-opti x (- n 1))))))

;; Redefinitions of some in built procedures for fun
(define (length-rec lst)
  (if (null? lst)
      0
      (+ 1 (length-rec (cdr lst)))))

(define (length-iter lst)
  (define (length-iter-impl acc rest)
    (if (null? rest)
        acc
        (length-iter-impl (+ 1 acc)
                          (cdr rest))))
  (length-iter-impl 0 lst))

;; Doing map manually
(define (sqr-all-rec lst)
  (if (null? lst)
    '()
    (let ((cur (car lst)))
      (cons (* cur cur)
            (sqr-all-rec (cdr lst))))))

(define (sqr-all-iter lst)
  (define (sqr-all-iter-impl rest acc)
    (if (null? rest)
      (reverse acc)
      (let ((cur (car rest)))
        (sqr-all-iter-impl (cdr rest)
                           (cons (* cur cur)
                                 acc)))))
  (sqr-all-iter-impl lst '()))

(define (sqr-all-destructive lst)
  (if (not (null? lst))
      (begin
        (set-car! lst (* (car lst) (car lst)))
        (sqr-all-destructive (cdr lst)))))

(define (abs-all lst)
  (if (null? lst)
    '()
    (cons (abs (car lst))
          (abs-all (cdr lst)))))

;; Higher order procedures
(define (my-map proc lst)
  (if (null? lst)
    '()
    (cons (proc (car lst))
          (my-map proc (cdr lst)))))

(define (my-map-2 proc lst1 lst2)
  (if (null? lst1)
    '()
    (cons (proc (car lst1)
                (car lst2))
          (my-map-2 proc (cdr lst1) (cdr lst2)))))

(define (my-map-n proc . lsts)
  (if (null? (car lsts))
    '()
    (cons (apply proc (my-map car lsts))
          (apply my-map-n proc (my-map cdr lsts)))))

(my-map (lambda (x) (* x x)) '(1 2 3))
(my-map-2 + '(1 2 3) '(1 2 3))
(my-map-n + '(1 2 3) '(4 5 6) '(7 8 9))
(my-map-n cons '(1 2 3) '(4 5 6) '(7 8 9))

(define (my-map-iter proc lst)
  (define (my-map-iter-impl rest acc)
    (if (null? rest)
      (reverse acc) ;; bad
      (my-map-iter-impl (cdr rest)
                        (cons (proc (car rest))
                              acc))))
  (my-map-iter-impl lst '()))

(define (my-reduce proc lst id)
  (if (null? lst)
    id
    (proc (car lst)
          (my-reduce proc (cdr lst) id))))

(define (my-list . args)
  (my-reduce cons args '()))

(define (dot-prod lst1 lst2)
  (if (null? lst1)
    0
    (+ (* (car lst1)
          (car lst2))
       (dot-prod (cdr lst1)
                 (cdr lst2)))))

(define (dot-prod-ho . lsts)
  (my-reduce + (apply my-map-n * lsts) 0))

(dot-prod-ho '(1 2 3) '(3 2 1))

(define (my-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst)
               (my-filter pred (cdr lst))))
        (else (my-filter pred (cdr lst)))))

(my-filter odd? '(1 2 3 4 5))
(my-filter (lambda (x)
             (and (> x 10)
                  (< x 100)))
           '(1 15 20 50 30 550 124))
(my-reduce cons '(1 2 3 4) '())

(define (percentages lst)
  (let ((lst-sum (my-reduce + lst 0)))
    (map (lambda (x) (* (/ x lst-sum) 100)) lst)))

(define (percentages-expanded lst)
  ((lambda (lst-sum)
     (map (lambda (x) (* (/ x lst-sum) 100)) lst))
   (my-reduce + lst 0)))

(define (nested-lets)
  ((lambda (x)
    ((lambda (y)
       (list x y))
     42))
    7))

(define (my-cons x y)
  (let ((cell (lambda (msg)
                (cond ((eq? msg 'x) x)
                      ((eq? msg 'y) y)))))
    (display "(") (display x) (display " . ") (display y) (display ")") (newline)
    cell));; scuffed

(define (my-car pair)
  (pair 'x))

(define (my-cdr pair)
  (pair 'y))

(define (my-cons-2 x y)
  (lambda (proc) (proc x y)))

(define (my-car-2 pair)
  (pair (lambda (x y) x)))

(define (my-cdr-2 pair)
  (pair (lambda (x y) y)))

;; Trees
(define (count-leaves-cheat tree)
  (length (flatten tree)))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((pair? tree)
         (+ (count-leaves (car tree))
            (count-leaves (cdr tree))))
        (else 1)))

(define tree '((1 2 4 4) 3 4))

(define (my-flatten tree)
  (cond ((null? tree) '())
        ((list? tree)
         (append (my-flatten (car tree))
                 (my-flatten (cdr tree))))
        (else (list tree))))

(define (my-append lst1 lst2)
  (define (my-append-iter iter acc)
    (if (null? iter)
      acc
      (my-append-iter (cdr iter)
                      (cons (car iter)
                             acc))))
  (my-append-iter (reverse lst1) lst2))

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((pair? tree)
         (cons (tree-map proc (car tree))
               (tree-map proc (cdr tree))))
        (else (proc tree))))

(define (tree-map-2 proc tree)
  (my-map (lambda (tree)
            (if (pair? tree)
              (tree-map-2 proc tree)
              (proc tree)))
          tree))

(tree-map-2 (lambda (x) (* x x)) '((1 2) 3 4))

;; sets
(define (element-of-set? element set)
  (cond ((null? set) #f)
        ((eq? element (car set)) #t)
        (else (element-of-set? element (cdr set)))))

(define (adjoin-set element set)
  (if (element-of-set? element set)
    set
    (cons element set)))

(define (intersection-set set1 set2)
  (define (intersection-set-impl iter acc)
    (cond ((null? iter) acc)
          ((element-of-set? (car iter) set2)
           (intersection-set-impl (cdr iter)
                                  (cons (car iter) acc)))
          (else (intersection-set-impl (cdr iter) acc))))
  (intersection-set-impl set1 '()))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))

(define my-set '(5 19 22 42))
(define my-set2 '(5 2 44 19))

;; Ordered list for set implementation (for numbers)
(define (element-of-set2? element set)
  (cond ((null? set) #f)
        ((= element (car set)) #t)
        ((< element (car set)) #f)
        (else (element-of-set2? element (cdr set)))))

(define my-set3 '(1 2 4 5 6))

;; Binary trees for set implementation (for numbers)
(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (element-of-set3? element set)
  (cond ((null? set) #f)
        ((= element (entry set)) #t)
        ((< element (entry set))
         (element-of-set3? element (left-branch set)))
        (else
          (element-of-set3? element (right-branch set)))))

(define (adjoin-set3 element set)
  (cond ((null? set) (make-tree element '() '()))
        ((= element (entry set)) set)
        ((< element (entry set))
         (make-tree (entry set)
                    (adjoin-set3 element (left-branch set))
                    (right-branch set)))
        (else
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set3 element (right-branch set))))))
