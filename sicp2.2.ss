;(define (list-ref items n)
;  (if (= n 0)
;      (car items)
;      (list-ref (cdr items) (- n 1))))
;
(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (mlength items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (mlength-itr items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (mappend list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;ex: 2.17
(define (mlast-pair l)
  (if (= (length l) 1)
      l
      (mlast-pair (cdr l))))
;ex: 2.18
(define (mreverse l)
  (define (mrev l r)
    (if (= (length l) 0)
        r
        (mrev (cdr l) (cons (car l) r))))
  (mrev l (list )))

;ex: 2.20
(define (same-parity p . l)
  (let ((parity (if (even? p) 
                    even?
                    odd?)))
    (define (parity-list l)
      (if (null? l)
          (list)
          (if (parity (car l)) 
              (cons (car l) (parity-list (cdr l)))
              (parity-list (cdr l)))))
    (cons p (parity-list l))))

;ex: 2.21
(define (square x) (* x x))
(define (square-list-1 items)
  (if (null? items)
      (list)
      (cons (square (car items)) 
            (square-list-1 (cdr items)))))
(define (square-list-2 items)
  (map square items))

;ex: 2.23
(define (mfor-each proc l)
  (cond ((null? l) (list))
        (else 
         (proc (car l))
         (mfor-each proc (cdr l)))))

;ex: 2.27
(define (deep-reverse l)
  (define (deep-rev-itr l r)
    (cond ((null? l) r)
          ((pair? (car l)) (deep-rev-itr (cdr l) (cons (deep-rev-itr (car l) '()) r)))
          (else (deep-rev-itr (cdr l) (cons (car l)  r)))))
  (deep-rev-itr l '()))

;ex: 2.28
(define (fringe l)
  (cond ((null? l) '())
        ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))

;ex: 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree) 
         (cond ((not (pair? sub-tree)) (proc sub-tree))
               (else (tree-map proc sub-tree))))
       tree))
;ex: 2.30
(define (square-tree l) (tree-map (lambda (x) (* x x)) l))

;ex: 3.32
(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; sequence operations
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;ex: 2.33
(define (mmap p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '()  seq))
(define (mappend seq1 seq2)
  (accumulate cons seq2 seq1))
(define (mlength sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;ex: 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

;ex: 2.35
(define (count-leaves t)
  (accumulate + 0 
              (map 
               (lambda (x) (if (not (pair? x)) 1 (count-leaves x))) 
               t)))

;ex: 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;ex: 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;ex: 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

;ex: 2.39
(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))
(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))



;ex: 2.40
(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list i j)) 
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; lame prime 
(define (prime? n)
  (define (prime-iter? n i)
     (cond ((= i n) #t)
           ((= (remainder n i) 0) #f)
           (else (prime-iter? n (+ i 1)))))
  (prime-iter? n 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum? (unique-pairs n))))

;ex: 2.41
(define (triplesum n s)
  (define (unique-triples n)
    (flatmap (lambda (i)
               (flatmap (lambda (j) 
                          (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))
  (define (sum-is-s? triple) (= (accumulate + 0 triple) s))
  (define (show-triple triple) (append triple (list (accumulate + 0 triple))))
  (map show-triple (filter sum-is-s? (unique-triples n))))

;ex: 2.42
(define (queens board-size)
  (define empty-board '())
  ;rather convoluted safe
  (define (safe? k positions)    
    (let ((kcol (cadr (list-ref positions (- k 1)))))
      (define (safe-with? pos rest)
        (let ((i (car pos))(j (cadr pos)))
          (cond ((= i k) #t)
                (else (and (not (or 
                                 (= (abs (- k i)) (abs (- j kcol)))
                                 (= j kcol)))
                           rest)))))
      (if (< k 2)
          #t
          (accumulate safe-with? #t positions))))
  (define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list (list k new-row))))
  
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

