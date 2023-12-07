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
(my-map-n + '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))
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

;; Destructive operations and procedure based object orientation:
;; Block structure with multiple procedures avoids variable parameters and apply
(define (make-account balance)
  (lambda (message . args)
    (cond ((eq? message 'withdraw)
           (if (<= (car args) balance)
             (begin
               (set! balance (apply - balance args))
               balance)
             "Insufficient funds"))
          ((eq? message 'deposit)
             (set! balance (apply + balance args))
             balance)
          ((eq? message 'balance)
           balance))))

(define (append-fun-iter lst1 lst2)
  (define (append-fun-iter-impl lst1 lst2)
    (if (null? lst1)
      lst2
      (append-fun-iter-impl (cdr lst1) (cons (car lst1) lst2))))
  (append-fun-iter-impl (reverse lst1) lst2))

(define (append-fun-rec lst1 lst2)
  (if (null? lst1)
    lst2
    (cons (car lst1) (append-fun-rec (cdr lst1) lst2))))

(define (append-imp lst1 lst2)
  (if (null? (cdr lst1))
    (set-cdr! lst1 lst2)
    ;; (set! lst1 123)) would not change anything
    (append-imp (cdr lst1) lst2)))

(define a '(1 2 3))
(define b '(4 5 6))

(append-imp a b)
(set! b '(7 8 9))
a ;; -> (1 2 3 4 5 6) ;; not affacted by changing b with `set!'
b ;; -> (7 8 9)

(define (make-queue queue)
  (define (add x)
    (define (add-iter q)
      (if (null? (cdr q))
        (set-cdr! q (cons x '()))
        (add-iter (cdr q))))
    (if (null? queue)
      (set! queue (cons x '()))
      (add-iter queue)))
  (define (pop)
    (if (null? queue)
      '()
      (let ((popped (car queue)))
        (set! queue (cdr queue))
        popped)))
  (lambda (message)
    (cond ((eq? message 'add) add)
          ((eq? message 'pop) pop)
          ((eq? message 'queue) queue))))

(define (q-add queue x)
  ((queue 'add) x))

(define (q-pop queue)
  ((queue 'pop)))

(define (q-get queue)
  (queue 'queue))

(define q (make-queue '()))
(q-get q)
(q-pop q)
(q-add q 4)
(q-add q 3)
(q-add q 2)

(define (i-cons x y)
  (lambda (m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) (lambda (v) (set! x v)))
          ((eq? m 'set-cdr!) (lambda (v) (set! y v))))))

(define (i-car p)
  (p 'car))

(define (i-cdr p)
  (p 'cdr))

(define (i-set-car! p v)
  ((p 'set-car!) v))

(define (i-set-cdr! p v)
  ((p 'set-cdr!) v))

(define a (i-cons 1 2))
(i-car a)
(i-set-car! a 5)

(define (sum-rec . args)
  (if (null? args)
    0
    (+ (car args)
       (apply sum-rec (cdr args)))))

(define (sum-iter . args)
  (define (sum-iter-impl rest acc)
    (if (null? rest)
      acc
      (sum-iter-impl (cdr rest)
                     (+ (car rest) acc))))
  (sum-iter-impl args 0))

(define (display+ . args)
  (for-each (lambda (m)
              (display m) (newline))
            args))

;; Associative lists (tables, hashmaps)
(define table '((a . 1) (b . 2) (c . 3)))

(define (assoc key table)
  (cond ((null? table) #f)
        ((eq? (car table) '*table*)
         (assoc key (cdr table)))
        ((equal? key (caar table))
         (car table))
        (else
          (assoc key (cdr table)))))

(define (make-table) (list '*table*))

(define (insert! key value table)
  (let ((record (assoc key table)))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value) (cdr table))))))

(define (remove! key table)
  (define (remove!-impl prev-rest rest)
    (cond ((null? rest) #f)
          ((equal? key (caar rest))
           (set-cdr! prev-rest (cdr rest)))
          (else (remove!-impl rest
                              (cdr rest)))))
  (if (null? (cdr table))
    #f
    (remove!-impl table
                  (cdr table))))

;; Anonymous recursion (below is implementation of (fac 5))
((lambda (proc n)
   (proc proc n))
   (lambda (proc n)
     (if (= n 1)
       1
       (* n (proc proc (- n 1))))) 5)

;; Streams:
(define the-empty-stream '())

(define (my-force promise) (promise))

(define-syntax my-delay
  (syntax-rules ()
    ((my-delay exp) (memoize (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail) (cons head (my-delay tail)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (my-force (cdr stream)))

(define (stream-null? stream)
  (null? stream))

(define (memoize proc)
  (let ((forced? #f)
        (result #f))
    (lambda ()
      (if (not forced?)
        (begin (set! result (proc))
               (set! forced? #t)))
      result)))


;; (define-syntax (my-delay x) (lambda () x))
;; (define (my-delay x) (lambda () x)) <- does not work because it will first evaluate x before storing delaying it?
;; (define-syntax my-if
;;   (syntax-rules (elseif else)
;;     ((if condition . body elseif body2 )

(my-force (cdr (cons 1 (my-delay (+ 1 2)))))

(stream-cdr (cons-stream 1 2))
(cdr (cons-stream 1 2))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))

(define (any? pred . targets)
  (cond ((null? targets) #f)
        ((pred (car targets)) #t)
        (else (apply any? pred (cdr targets)))))

(define (fin-stream-map proc . streams)
  (if (apply any? stream-null? streams)
    the-empty-stream
    (cons-stream (apply proc (map stream-car streams))
                 (apply fin-stream-map proc (map stream-cdr streams)))))

(define (inf-stream-map proc . streams)
  (cons-stream (apply proc (map stream-car streams))
               (apply inf-stream-map proc (map stream-cdr streams))))

(define (stream-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low (stream-interval (+ low 1) high))))

(define (show-stream stream . n)
  (define (show-stream-impl rest i)
    (cond ((= i 0) (display "...") (newline))
          ((stream-null? rest) (newline))
          (else
            (display (stream-car rest))
            (display " ") (show-stream (stream-cdr rest) (- i 1)))))
  (show-stream-impl stream (if (null? n) 15 (car n))))

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define nats (integers-from 1))
(define evens (cons-stream 0 (stream-filter even? nats)))
(define odds (stream-filter odd? nats))

(integers-from 2)
(even-numbers)
(stream-interval 1 99999999999999999999999999999999999)

(define fibs (cons-stream 0 (cons-stream 1 (inf-stream-map + fibs (stream-cdr fibs)))))
(show-stream fibs)

(define (powers-of base)
  (define (powers-of-impl exponent)
    (cons-stream exponent (powers-of-impl (* base exponent))))
  (cons-stream 1 (powers-of-impl base)))

(define power-table (inf-stream-map powers-of nats))
(show-stream power-table)

(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (stream-append (stream-cdr s1) s2))))

(define (stream-interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (stream-interleave s2 (stream-cdr s1)))))

(show-stream (stream-interleave evens odds))
(show-stream fibs)

;; More random stuff
(define (my-reverse lst)
  (let loop ((rest lst)
             (acc '()))
    (if (null? rest)
      acc
      (loop (cdr rest)
            (cons (car rest) acc)))))

;; exams:
;; 2022:
;;; 1a:
;;; Memoisering er nyttig for rent funksjonelle programmer siden det lar oss enkelt
;;; aksessere resultater av prosedyrekall ved gitte parametre, uten å måtte kalkulere
;;; hele prosedyren på nytt. Dette lar seg gjøre for rent funksjonelle programmer siden 
;;; i et slikt paradigme vil prosedyrer med samme parametre alltid gi samme returverdi.
;;; På den andre siden gjelder dette ikke for prosedyrer med bieffekter siden vi da ikke
;;; kan garantere at prosedyrekall alltid vil gi samme returverdi til enhver tid. Derfor 
;;; vil å lagre returverdier og returnere en memoisert verdi ikke alltid gi riktig svar 
;;; avhengig av når den kalles.

;;; 2a:
(define one (list 1))
(set-cdr! one 7)
one ;; -> (1 . 7)

;;; 2b:
(define foo '(1 2 3))
(let ((bar foo))
  (set! bar (cons 17 (cdr foo))))
foo ;; -> (1 2 3)

;;; 2c:
(define foo '(1 2 3))
(let ((baz foo))
  (set-cdr! baz (cons 17 (cdr foo))))
foo ;; -> (1 17 2 3)

;;; 3:
;;; 3a:
(define (reverse-all lst)
  (reverse
    (map (lambda (elem)
           (if (list? elem)
             (reverse-all elem)
             elem)) lst)))

(reverse-all (list 1 (list 2 3) (list 4 5)))

;;; 3b:
;;; (list)

;;; 4:
;;; 4a:
(define (magic n)
  (define (magic-iter n acc)
    (lambda (x)
      (if (= n 1)
        (+ acc x)
        (magic-iter (- n 1) (+ acc x)))))
  (magic-iter n 0))

;;; 5:
;;; 5a:
(define (height tree)
  (if (leaf? tree)
    0
    (max (+ 1 (height (left-branch tree))
         (+ 1 (height (right-branch tree)))))))

;; Returnerer lst2 om listene er like lange,
;; men oppgaveteksten sier hva vi gjør her ikke er viktig
(define (longest-list lst1 lst2)
  (if (> (length lst1) (length lst2))
    lst1
    lst2))

(define (longest-path tree)
  (if (leaf? tree)
    (entry tree)
    (longest-list (cons (entry tree) (longest-path (left-branch tree)))
                  (cons (entry tree) (longest-path (right-branch tree))))))

;;; 6:
;;; 6a:
(define (stream-diff-1 stream)
  (let ((stream-offset (stream-cdr stream)))
    (stream-map - stream stream-offset)))

;;; 6b:
(define (stream-diff n stream)
  (if (= n 0)
    stream
    (stream-diff (- n 1) (stream-diff-1 stream))))

;;; 6c:
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define (stream-deriv n proc)
  (let loop ((n-iter n)
             (mapped-s (stream-map proc (integers-from 0))))
    (if (= n-iter 0)
      mapped-s
      (loop (- n-iter 1) (stream-map - (stream-cdr mapped-s) mapped-s)))))

(define s0 (stream-deriv 0 (lambda (x) (* x x))))
(define s1 (stream-deriv 1 (lambda (x) (* x x))))
(show-stream s1 5)

;;; 6d:
(define (stream-of-list lst)
  (define (stream-of-list-impl lst-iter)
    (if (null? lst-iter)
      (stream-of-list-impl lst)
      (cons-stream (car lst-iter)
                   (stream-of-list-impl (cdr lst-iter)))))
  (if (null? lst)
    the-empty-stream
    (stream-of-list-impl lst)))

;;; 7:
(define (make-monitor proc)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-often) calls)
            ((eq? arg 'reset!) (set! calls 0))
            (else
              (set! calls (+ calls 1))
              (proc arg))))))

(define (make-account balance password)
  (let ((pass-fails 0)
        (blocked #f))
    (lambda (to-deposit in-password)     ;; We simplify the account to one lambda since we-
      (if (or (<= 3 pass-fails) blocked) ;; dont need to implement the other functionalities
        (begin (set! blocked #t)
               (error "account blocked")) ;; or (display ...)
        (if (eq? in-password password)
          (begin (set! pass-fails 0)
                 (set! balance (+ balance to-deposit))
                 balance)
          (begin (set! pass-fails (+ pass-fails 1))
                 (error "wrong password"))))))) ;; or (display ...)

;; better
(define (make-account balance password)
  (let ((pass-fails 0))
    (lambda (to-deposit in-password)
      (cond ((>= pass-fails 3)
             (error "account blocked"))
            ((eq? in-password password)
             (set! balance (+ balance to-deposit))
             balance)
            (else
              (set! pass-fails (+ pass-fails 1))
              (error "wrong password"))))))

(define (deposit account amount password)
  (account amount password))

(define myaccount (make-account 100 'qwerty))
(deposit myaccount 50 'qwerty)

;; 2023 prøveeksamen:
;;; 2a:
(define (remove-first pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cdr lst))
        (else
          (cons (car lst) (remove-first pred (cdr lst))))))

(define (atom? x)
  (not (list? x)))

(define (flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else
          (list lst))))

(define (map+ proc lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (cons (map+ proc (car lst)) (map+ proc (cdr lst))))
        (else
          (cons (proc (car lst)) (map+ proc (cdr lst))))))

(define (repeat proc n init)
  (if (= n 0)
    init
    (repeat proc (- n 1) (proc init))))

(define (repeat2 proc n)
  (lambda (init)
    (repeat proc n init)))

(define (repeat2-2 proc n)
  (lambda (init)
    (let loop ((acc init))
      (if (= n 0)
        acc
        (loop (proc acc))))))

(repeat (lambda (x) (* x 2)) 3 5)

(define (curry proc)
  (lambda (x)
    (lambda (y)
      (proc x y))))

(define (undo-cycle-imp lst)
  (define (iter slow fast)
    (cond ((equal? slow fast) ;; we assume input is always circular so never check for nulls
           (set-cdr! slow '()))
          (else
            (iter (cdr slow) (cddr fast)))))
  (iter lst (cdr lst)))

(define (undo-cycle-fun lst)
  (define (iter slow fast acc)
    (cond ((equal? slow fast)
           (reverse (cons (car slow) acc)))
          (else
            (iter (cdr slow) (cddr fast) (cons (car slow) acc)))))
  (iter lst (cdr lst) '()))

(define a '(1 2 3))
(define b (cdr (cdr a)))
(set-cdr! b a)
(undo-cycle-imp a)

;;; 6a:
(define (tree-mirror tree)
  (if (leaf? tree)
    tree
    (make-tree (entry tree)
               (right-branch tree)
               (left-branch tree))))
;;; 6b:
(define (tree-map proc tree)
  (if (leaf? tree)
    (make-leaf (proc (entry tree)))
    (make-tree (proc (entry tree))
               (tree-map (left-branch tree))
               (tree-map (right-branch tree)))))

;;; 7;
;;; 7a:
(define (stream-merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        ((>= (stream-car s1) (stream-car s2)) ;; doesnt specify what to do when equal, so-
         (cons-stream (stream-car s1)         ;; assume s1 takes priority over s2 with >=
                      (stream-merge (stream-cdr s1) s2)))
        (else
          (cons-stream (stream-car s2)
                       (stream-merge s1 (stream-cdr s2))))))

;;; 7b:
;;; Å implementere sorteringsalgoritmer som jobber på strømmer er mulig, så lenge strømmen
;;; er endelig. For uendelige strømmer går det ikke an å sortere dem siden vi ikke kan vite
;;; om det finnes et lavere element senere i strømmen (eller hvor mange av de samme elementene
;;; vi har). Selv om det er mulig å sortere strømmer er dette en ganske meningsløs operasjon,
;;; siden det krever at vi må evaluere hele strømmen for å kunne sammenligne verdiene, og på
;;; det punktet kunne vi like godt bare hatt en liste siden vi ikke lenger får noenting ut av
;;; å lagre datastrukturen som en strøm. I tillegg er det verdt å nevne at generelt sett vil
;;; sortering av både strømmer og lister i språk som scheme være relativt ueffektivt i forhold
;;; til andre imperative språk ettersom schemes lister er lenkede lister framfor lettere
;;; indekserbare arrays. Med det mener jeg at siden sorteringsalgoritmer ofte avhenger av å
;;; utføre mange indekseringer, på vilkårlige steder i listen vil vi i scheme måtte traversere
;;; unødvendig mange ganger gjennom samme listen hvis vi for eksempel vil flytte liste[5] til
;;; liste[6]. Dette kan til en viss grad optimeres dersom man bruker destruktive operasjoner
;;; som set-car! eller set-cdr!, men vil fortsatt være relativt tregt.

;;; 8:
(define (make-empty-stack)
  (let ((stack '()))
    (define (push x)
      (set! stack (cons x stack)))
    (define (pop)
      (if (empty?) ;; (null? stack)
        (display "empty stack!\n")
        (let ((popped (car stack)))
          (set! stack (cdr stack))
          popped)))
    (define (empty?) ;; not in task description
      (null? stack))
    (lambda (message) ;; alternatively: define as "dispatch" procedure and return it
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'stack) stack)
            ((eq? message 'empty?) empty?)))))

(define (pop s)
  ((s 'pop)))

(define (push s x)
  ((s 'push) x))

(define (empty? s)
  ((s 'empty?)))

(define (stack s) ;; also not in task description
  (s 'stack))
