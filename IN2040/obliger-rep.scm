;; 1a
(define (sign-if x)
  (if (positive? x)
    1
    (if (negative? x)
      -1
      0)))

(define (sign-and-or x)
  (or (and (positive? x) 1)
      (and (negative? x) -1)
      0))

(define (add1 x)
  (+ 1 x))

(define (sub1 x)
  (- 1 x))

(define (plus x y)
  (if (zero? x)
    y
    (plus (sub1 x) (add1 y))))

(define (minus x y)
  (if (zero? y)
    x
    (minus (sub1 x) (sub1 y))))

(define (plus-rec x y)
  (if (zero? x)
    y
    (add1 (plus-rec (sub1 x) y))))

(define (minus-rec x y)
  (if (zero? y)
    x
    (sub1 (minus-rec x (sub1 y)))))

(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
      e
      (power-iter (+ 1 e))))
  (power-iter 1))

;; 1b
((lambda (l) (cadr l)) '(0 42 #t bar))
((lambda (l) (cadar l)) '((0 42) (#t bar)))
((lambda (l) (caadr l)) '((0) (42 #t (bar))))

(list '(0 42) '(#t bar))
(cons (cons 0 (cons 42 '())) (cons (cons #t (cons 'bar '())) '()))

(define (take-rec n items)
  (if (= n 0)
    '()
    (cons (car items)
          (take-rec (- n 1)
                (cdr items)))))

(define (take-iter n items)
  (define (take-iter-impl n rest acc)
    (if (= n 0)
      (reverse acc)
      (take-iter-impl (- n 1)
                 (cdr rest)
                 (cons (car rest) acc))))
  (take-iter-impl n items '()))

(define (take-while pred items)
  (let ((cur (car items)))
    (if (pred cur)
      (cons cur (take-while pred (cdr items))) 
      '())))

(define (map2 proc lst1 lst2)
    (if (or (null? lst1)
            (null? lst2))
      '()
      (cons (proc (car lst1)
                  (car lst2))
            (map2 proc (cdr lst1) (cdr lst2)))))

(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))

;; 2a:
(load "huffman.scm")

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car pair)
  (pair (lambda (x y) x)))

(define (p-cdr pair)
  (pair (lambda (x y) y)))

(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))

(define foo 42)

((lambda (foo x)
   (if (= x foo)
     'same
     'different))
   5 foo)

((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel)

(define (infix-eval exp)
  (let ((exp1 (car exp))
        (operand (cadr exp))
        (exp2 (caddr exp)))
    (operand exp1 exp2)))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo)
(infix-eval baz)
(infix-eval bar)

;; error siden '/ ikke er en prosedyre
(define bah '(84 / 2))
(infix-eval (list '84 / '2))

(decode sample-code sample-tree)

(define (decode-iter bits tree)
  (define (decode-iter-impl bits current-branch message)
    (if (null? bits)
      (reverse message)
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (decode-iter-impl (cdr bits) tree (cons (symbol-leaf next-branch)
                                                  message))
          (decode-iter-impl (cdr bits) next-branch message)))))
  (decode-iter-impl bits tree '()))

(define (encode message tree)
  (define (encode-impl message current-branch code)
    (if (null? message)
      (reverse code)
      (cond ((leaf? current-branch)
             (encode-impl (cdr message) tree code))
            ((memq (car message) (symbols (left-branch current-branch)))
             (encode-impl message (left-branch current-branch) (cons 0 code)))
            (else
              (encode-impl message (right-branch current-branch) (cons 1 code))))))
  (encode-impl message tree '()))

(define (grow-huffman-tree freqs)
  (define (grow-huffman-tree-impl freqs)
    (if (null? (cdr freqs))
        (car freqs)
        (grow-huffman-tree-impl (adjoin-set (make-code-tree (car freqs)
                                                            (cadr freqs))
                                            (cddr freqs)))))
  (grow-huffman-tree-impl (make-leaf-set freqs)))

(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode-iter (encode '(a b c) codebook) codebook)

(define freqs '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3)
                (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by 12)
                (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))
(define message '(ninjas fight ninjas fight ninjas ninjas fight
                         samurais samurais fight samurais fight ninjas
                         ninjas fight by night))
(define codebook (grow-huffman-tree freqs))

(define (weights tree)
  (if (leaf? tree)
    (list (weight tree))
    (append (weights (left-branch tree))
            (weights (right-branch tree)))))

(define (huffman-leaves tree)
  (map list (symbols tree) (weights tree)))

(weights sample-tree)
(huffman-leaves sample-tree)

;; 2b:
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define (make-stack stack)
  (lambda (proc . args)
    (cond ((eq? proc 'push!)
           (set! stack (append args stack)))
          ((eq? proc 'pop!)
           (if (not (null? stack))
             (set! stack (cdr stack))))
          ((eq? proc 'stack)
           stack))))

(define (push! s . args)
  (apply s 'push! args))

(define (pop! s)
  (s 'pop!))

(define (stack s)
  (s 'stack))

;; 3a:
(define mem
  (let ((procs (make-table)))
    (lambda (message proc)
      (cond ((eq? 'unmemoize message)
             (lookup proc procs))
            ((eq? 'memoize message)
             (let* ((results (make-table))
                    (memoized-proc
                      (lambda args
                        (let ((memoized (lookup args results)))
                          (or memoized ;; effectively (if memoized memoized ...)
                            (let ((result (apply proc args)))
                              (insert! args result results)
                              result))))))
               (insert! memoized-proc proc procs)
               memoized-proc))))))
