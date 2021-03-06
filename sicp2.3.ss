;section 2.3.1
(define (my-memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;ex: 2.54
(define (my-equal? la lb)
  (cond ((and (null? la) (null? lb)) 
         #t)
        ((and (not (pair? (car la))) 
              (not (pair? (car lb)))
              (eq? (car la) (car lb)))
         (my-equal? (cdr la) (cdr lb)))
        ((and (pair? (car la))
              (pair? (car lb)))
         (and (my-equal? (car la) (car lb))
              (my-equal? (cdr la) (cdr lb))))
        (else #f)))


;section 2.3.2
(define (deriv exp var)
;  (display "new exp ")(newline) (display exp)(newline)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
;         (display "sum ")(display (addend exp)) (display " + ") (display (augend exp)) (newline)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
;         (display "product ")(display (multiplier exp)) (display " * ") (display (multiplicand exp))(newline)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponent? exp)
         (make-product (exponent exp) 
                       (make-product (make-exponentiation 
                                      (base exp) (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (display "unknown expression type -- DERIV") (display exp) (newline))))

;representation
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
;(define (sum? x)
;  (and (pair? x) (eq? (car x) '+)))
;(define (addend s) (cadr s))
;(define (augend s) (caddr s))
;(define (augend s) 
;  (define (make-augend-sum a)
;    (if (not (pair? (cdr a)))
;        (car a)
;        (make-sum (car a) (make-augend-sum (cdr  a)))))
;  (make-augend-sum (cddr s)))

;(define (product? x)
;  (and (pair? x) (eq? (car x) '*)))
;(define (multiplier p) (cadr p))
;(define (multiplicand p) (caddr p))
;(define (multiplicand p)
;  (define (make-multiplicand-sum a)
;    (if (not (pair? (cdr a)))
;        (car a)
;        (make-product (car a) (make-multiplicand-sum (cdr a)))))
;  (make-multiplicand-sum (cddr p)))

;ex: 2.56
(define (exponent? x) 
  (and (pair? x) (eq? (car x) '^)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (=number? base) (=number? exponent))
         (expt base exponent))
        (else (list '^ base exponent))))

;ex: 2.57
(define (sum? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) 
  (define (after-mul s) (cdddr s))
  (cond ((not (> (length (cddr s)) 2)) (caddr s))
        ((and (product? (cddr s)) (not (null? (after-mul (cddr s)))))
         (let ((p (cddr s)))
;           (newline) (display (after-mul p))(newline)
;           (newline) (display (cons (list  (multiplier p) '* (multiplicand p)) (cons '+ (after-mul s)))) (newline)
           (cons (list  (multiplier p) '* (multiplicand p)) (cons '+ (after-mul p)))))
        (else (cddr s))))

(define (product? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;(deriv '(x + 3 * (x + y + 2)) 'x)
;(deriv '(x + x + y + x) 'x)
;(deriv '(x * ( x + y)) 'x)


;section: 2.3.3
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;ex: 2.59
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (cons (car s) (union-set (cdr s) s2)))))

;ex: 2.60
(define d-element-of-set? element-of-set?)
(define d-adjoin-set cons)
(define d-intersection-set intersection-set)
(define d-union-set append)


;ordered set representation
(define (o-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(define (o-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))
;ex: 2.61
(define (o-adjoin-set x s)
  (cond ((null? s) (list x))
        ((= x (car s)) s)
        ((< x (car s)) (cons x s))
        (else (cons (car s) (o-adjoin-set x (cdr s))))))
;ex: 2.62
(define (o-union-set s1 s2)
  (cond ((null? s1) s2) ((null? s2) s1)
        (else 
         (let ((x (car s1)) (y (car s2)) (xx (cdr s1)) (yy (cdr s2)))
           (cond 
             ((= x y) (cons x (o-union-set xx yy)))
             ((< x y) (cons x (o-union-set xx s2)))
             (else (cons y (o-union-set s1 yy))))))))

;binary tree representation
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (t-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (t-adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;ex: 2.65
(define (t-union-set s1 s2)
  (let ((os1 (tree->list-2 s1))
        (os2 (tree->list-2 s2)))
    (list->tree (o-union-set os1 os2))))
(define (t-intersection-set s1 s2)
  (let ((os1 (tree->list-2 s1))
        (os2 (tree->list-2 s2)))
    (list->tree (o-intersection-set os1 os2))))

;ex: 2.66
(define (lookup t v)
  (cond ((null? t) #f)
        ((= (entry t) v) #t)
        ((< v (entry t)) (lookup (left-branch t) v))
        (else (lookup (right-branch t) v))))

;section 2.3.4
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
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


;ex: 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;ex: 2.68
(define (include? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (include? x (cdr l)))))
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol x t)
  (cond 
;    ((leaf? t) 
;     (if (eq? x (symbol-leaf t))
;         '()
;         (display "TF leaf")))
    ((leaf? t) '())
    ((include? x (symbols (left-branch t))) (cons 0 (encode-symbol x (left-branch t))))
    ((include? x (symbols (right-branch t))) (cons 1 (encode-symbol x (right-branch t))))
    (else (display "TF"))))

;ex: 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge l)
;  (display l)(display (length l))(newline)
  (cond ((= (length l) 1) (car l))
        (else (successive-merge (adjoin-set (make-code-tree (car l) (cadr l)) (cddr l))))))
;test
;ex: 2.70
(define sample270-tree (generate-huffman-tree '((a 2) (boom  1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1))))
(define sample270-message '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

