(require errortrace)

(load "ch4-ambeval.scm")

(define (def exp)
  (ambeval exp the-global-environment (lambda (val fail) 'success) (lambda () 'failure)))

(def '(define (require p)
        (if (not p)
            (amb)
            p)))


(def '(define (an-element-of items)
        (require (not (null? items)))
        (amb (car items) (an-element-of (cdr items)))))

(def '(define (an-integer-starting-from n)
        (amb n (an-integer-starting-from (+ n 1)))))

;; ;; ex: 4.35
(def '(define (an-integer-between l h)
        (require (<= l h))
        (amb l (an-integer-between (+ l 1) h))))

(def '(define (a-pythagorean-triple-between low high)
        (let ((i (an-integer-between low high)))
          (let ((j (an-integer-between i high)))
            (let ((k (an-integer-between j high)))
              (require (= (+ (* i i) (* j j)) (* k k)))
              (list i j k))))))


;; ex: 4.36
(def '(define (amb-pythagorean-triples)
        (define (amb-pythagorean-triples-int low sum)
          (let ((i (an-integer-between low (- sum 2))))
            (let ((j (an-integer-between i (- sum 1))))
              (let ((k (- sum i j)))
                (require (= (+ (* i i) (* j j)) (* k k)))
                (list i j k sum)))))
        (define (amb-pythagorean-triples-rec sum)
          (amb (amb-pythagorean-triples-int 1 sum)
               (amb-pythagorean-triples-rec (+ sum 1))))
        (amb-pythagorean-triples-rec 3)))


(def '(define (a-pythagorean-triple-between low high)
        (let ((i (an-integer-between low high))
              (hsq (* high high)))
          (let ((j (an-integer-between i high)))
            (let ((ksq (+ (* i i) (* j j))))
              (require (>= hsq ksq))
              (let ((k (sqrt ksq)))
                (require (integer? k))
                (list i j k)))))))

;; section 4.3.2
(def '(define (distinct? items)
        (cond ((null? items) true)
              ((null? (cdr items)) true)
              ((member (car items) (cdr items)) false)
              (else (distinct? (cdr items))))))

(def '(define (multiple-dwelling)
        (let ((baker (amb 1 2 3 4 5))
              (cooper (amb 1 2 3 4 5))
              (fletcher (amb 1 2 3 4 5))
              (miller (amb 1 2 3 4 5))
              (smith (amb 1 2 3 4 5)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= baker 5)))
          (require (not (= cooper 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require (> miller cooper))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))

;; ex: 4.38
(def '(define (multiple-dwelling-38)
        (let ((baker (amb 1 2 3 4 5))
              (cooper (amb 1 2 3 4 5))
              (fletcher (amb 1 2 3 4 5))
              (miller (amb 1 2 3 4 5))
              (smith (amb 1 2 3 4 5)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= baker 5)))
          (require (not (= cooper 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require (> miller cooper))
          ;; (require (not (= (abs (- smith fletcher)) 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))

;; ex: 4.39
(def '(define (multiple-dwelling-39)
        (let ((baker (amb 1 2 3 4 5))
              (cooper (amb 1 2 3 4 5))
              (fletcher (amb 1 2 3 4 5))
              (miller (amb 1 2 3 4 5))
              (smith (amb 1 2 3 4 5)))
          (require (not (= baker 5)))
          (require (not (= cooper 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require (> miller cooper))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))

;; random problem from web
(define (group-similar grouped ol)
  (cond
   ((null? ol) '())
   ((= (car grouped) (car ol))
    (group-similar
     (cons (car ol) grouped)
     (cdr ol)))
   (else
    (cons grouped
          (group-similar
           (cons (car ol) '())
           (cdr ol))))))

(group-similar '(1) '(4 4 8 1 9 1 1 1 2 2 3))

;; ex: 4.40
(def '(define (multiple-dwelling-40)
         (let ((fletcher (amb 2 3 4)))
          (let ((cooper (amb 2 3 4 5)))
            (require (not (= (abs (- fletcher cooper)) 1)))
            (let ((miller (amb 3 4 5)))
              (require (> miller cooper))
              (let ((smith (amb 1 2 3 4 5)))
                (require (not (= (abs (- smith fletcher)) 1)))
                (let ((baker (amb 1 2 3 4)))
                  (require (distinct? (list baker cooper fletcher miller smith)))
                  (list (list 'baker baker)
                      (list 'cooper cooper)
                      (list 'fletcher fletcher)
                      (list 'miller miller)
                      (list 'smith smith)))))))))

;; ex: 4.41
(define (ints-in-range a b)
  (if (> a b)
      '()
      (cons a (ints-in-range (+ a 1) b))))
(define (rm c l)
        (remove c l))
(define (rml cl l)
  (if (null? cl)
      l
      (rml (cdr cl) (remove (car cl) l))))
(define (multiple-dwelling-41)
  (let ((locs (list 1 2 3 4 5)))
    (define (baker)
      (let ((bakers-choices (remove 5 locs)))
        (filter (lambda (x) (cooper (list x))) bakers-choices)))
    (define (cooper alloc)
      (let ((coopers-choices (rml (cons 1 alloc) locs)))
        (filter (lambda (x) (fletcher (cons x alloc))) coopers-choices)))
    (define (fletcher alloc)
      (let ((fletchers-choices (rml (append (list 1 5 (+ (car alloc) 1) (- (car alloc) 1)) alloc) locs)))
        (filter (lambda (x) (smith (cons x alloc))) fletchers-choices)))
    (define (smith alloc)
      (let ((smith-choices (rml (append (list (+ (car alloc) 1) (- (car alloc) 1)) alloc) locs)))
        (filter (lambda (x) (miller (cons x alloc))) smith-choices)))
    (define (miller alloc)
      (let ((miller-choices (rml alloc (ints-in-range (caddr alloc) 5))))
        (filter (lambda (x)
                  (display (list "sol: " (reverse (cons x alloc))))
                  (newline)
                  (reverse (cons x alloc)))
                miller-choices)))
    (baker)))

;; ex: 4.42
(def '(define (liars)
        (define (xor a b) (or (and (not a) b) (and a (not b))))
        (let ((betty (amb 1 2 3 4 5))
              (ether (amb 1 2 3 4 5))
              (joan (amb 1 2 3 4 5))
              (kitty (amb 1 2 3 4 5))
              (mary (amb 1 2 3 4 5)))
          (require (distinct? (list betty ether joan kitty mary)))
          ;; (newline)(display (list betty ether))(newline)
          (require (xor (= kitty 2) (= betty 3)))
          (require (xor (= ether 1) (= joan 2)))
          (require (xor (= kitty 2) (= mary 4)))
          (require (xor (= mary 4) (= betty 1)))
          (list (list 'betty betty)
                (list 'ether ether)
                (list 'joan joan)
                (list 'kitty kitty)
                (list 'mary mary)))))

;; ex: 4.43
(def '(define (yacht)
        (define (fathers-y g l)
          (cond ((null? l) false)
                ((= g (car (car l)))
                 (cdr (car l)))
                (else (fathers-y g (cdr l)))))
        (define girls (list 'lora 'melissa 'rosalind 'gabrielle 'ann))
        (define (get-girl n)
          (define (get-girl-int n girls)
            (if (= n 1)
                (car girls)
                (get-girl-int (- n 1) (cdr girls))))
          (get-girl-int (car n) girls))
        
        (let ((moore (cons 5 1))
              (barnacle (cons 2 4))
              (downing (cons (amb 1 3 4) 2)))
          (let ((hall (cons (amb 1 4) 3)))
            (let ((parker (cons (amb 1 3) 5)))
              (require (distinct? (list (car moore) (car barnacle) (car downing) (car hall) (car parker))))
              (require (= (fathers-y 4 (list moore barnacle downing hall parker)) (car parker)))
              (list (list 'moore (get-girl moore))
                    (list 'barnacle (get-girl barnacle))
                    (list 'downing (get-girl downing))
                    (list 'hall (get-girl hall))
                    (list 'parker (get-girl parker))))))))

(def '(define (yacht-ann)
        (define (fathers-y g l)
          (cond ((null? l) false)
                ((= g (car (car l)))
                 (cdr (car l)))
                (else (fathers-y g (cdr l)))))
        (define girls (list 'lora 'melissa 'rosalind 'gabrielle 'ann))
        (define (get-girl n)
          (define (get-girl-int n girls)
            (if (= n 1)
                (car girls)
                (get-girl-int (- n 1) (cdr girls))))
          (get-girl-int (car n) girls))
        
        (let ((moore (cons (amb 3 4 5) 1))
              (barnacle (cons 2 4))
              (downing (cons (amb 1 3 4 5) 2)))
          (let ((hall (cons (amb 1 4 5) 3)))
            (let ((parker (cons (amb 1 3 5) 5)))
              (require (distinct? (list (car moore) (car barnacle) (car downing) (car hall) (car parker))))
              (require (= (fathers-y 4 (list moore barnacle downing hall parker)) (car parker)))
              (list (list 'moore (get-girl moore))
                    (list 'barnacle (get-girl barnacle))
                    (list 'downing (get-girl downing))
                    (list 'hall (get-girl hall))
                    (list 'parker (get-girl parker))))))))

;; ex: 4.44
(def '(define (member? e l)
        (cond ((null? l) false)
              ((eq? e (car l)) true)
              (else (member? e (cdr l))))))
(def '(define (map fn l)
      (if (null? l)
          '()
          (cons (fn (car l)) (map fn (cdr l))))))
(def
 '(define (8-queens)
    (define (not-diagonal n p alloc)
      (define (not-diagonal-int n cur level alloc)
        (cond ((< level 1) true)
              ((= (abs (- n level)) (abs (- cur (car alloc))))
               false)
              (else (not-diagonal-int n cur (- level 1) (cdr alloc)))))
      (not-diagonal-int n p (- n 1) alloc))
    
    (define (8-queens-int n alloc)
      (if (> n 8)
          alloc
          (begin
            (let ((choice (amb 1 2 3 4 5 6 7 8)))
              (require (not (member? choice alloc)))
              (require (not-diagonal n choice alloc))
              (8-queens-int (+ n 1) (cons choice alloc))))))
    (8-queens-int 1 '())))
          

          
;; http://vaguevagaries.blogspot.com/2008/06/99-lisp-problems-some-are-hard.html
(define (group-similar grouped ol)
  (cond
   ((null? ol) (list grouped))
   ((= (car grouped) (car ol))
    (group-similar
     (cons (car ol) grouped)
     (cdr ol)))
   (else
    (cons grouped
          (group-similar
           (cons (car ol) '())
           (cdr ol))))))

(group-similar '(1) '(1 2 3 3 3 3 3 3 3))
(group-similar '(1) '(4 4 8 1 9 1 1 1 2 2 3))
(group-similar '(1) '(1 1 1 1))
(group-similar '(2) '(3 4 5))

;; ex: 4.50
;; when testing, rename it to analyze-amb
(define (rand-list-ele lst)
  (list-ref lst (random (length lst))))
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((ele (rand-list-ele choices)))
              (ele env
                   succeed
                   (lambda ()
                     (try-next (remove ele choices)))))))
      (try-next cprocs))))

;; ex: 4.51
;; ((permanent-set? exp)
;;  (analyze-permanent-set exp))
;; add the above fn to analyze

(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-set exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

;; (define count 0)
;; (let ((x (an-element-of (quote (a b c))))
;;       (y (an-element-of (quote (a b c)))))
;;   (permanent-set! count (+ count 1))
;;   (require (not (eq? x y)))
;;   (list x y count))


;; ex: 4.52
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (analyze-if-fail exp)
  (let ((pred (analyze (if-predicate exp)))
        (altr (analyze (if-consequent exp))))
    (lambda (env succeed fail)
      (pred env
            succeed
            (lambda () (altr env succeed fail))))))

;; (if-fail (let ((x (an-element-of (quote (1 3 5)))))
;;            (require (even? x))
;;            x)
;;          'all-odd)

;; ex: 4.53
(def '(define (prime-sum-pair list1 list2)
        (let ((a (an-element-of list1))
              (b (an-element-of list2)))
          (require (prime? (+ a b)))
          (list a b))))
(def '(define (prime? n)
        (define (prime-iter? n i)
          (cond ((= i n) true)
                ((= (remainder n i) 0) false)
                (else (prime-iter? n (+ i 1)))))
        (prime-iter? n 2)))
;; (let ((pairs (quote ())))
;;   (if-fail (let ((p (prime-sum-pair (quote (1 3 5 8)) (quote (20 35 110)))))
;;              (permanent-set! pairs (cons p pairs))
;;              (amb))
;;            pairs))

;; ex: 4.54
(def '(define (even? x)
        (= (remainder x 2) 0)))
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (value fail2)
               ;; (newline)(display (list "req: " value fail2))
               (if (not (true? value))
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
