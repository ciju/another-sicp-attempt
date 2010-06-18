(load "debug.ss")

;; (require scheme/mpair)

(define *show-debug* #t)

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-null? s)
  (null? s))
(define the-empty-stream
  '())

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (proc (stream-car s))
            (delay (stream-map proc (stream-cdr s))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons (stream-car stream)
               (delay (stream-filter pred
                                     (stream-cdr stream)))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (display-n n s)
  (if (= n 0)
      (display-line (stream-car s))
      (begin (display-line (stream-car s))
             (display-n (- n 1) (stream-cdr s)))))

;; alread implemented with delay
;; (define (memo-proc proc)
;;   (let ((already-run? false) (result false))
;;     (lambda ()
;;       (if (not already-run?)
;;           (begin (set! result (proc))
;;                  (set! already-run? true)
;;                  result)
;;           result))))


;; ex: 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons
       (apply proc (map stream-car argstreams))
       (delay (apply stream-map
                     (cons proc (map stream-cdr argstreams)))))))

;; ex: 3.51
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons
       low
       (delay (stream-enumerate-interval (+ low 1) high)))))
(define (show x)
  (display-line x)
  x)
;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; (stream-ref x 5)
;; (stream-ref x 7)


;; ex: 3.52
;; (define sum 0)
;; (define (accum x)
;;   (set! sum (+ x sum))
;;   (display-line (cons x sum))
;;   sum)
;; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; (display-line sum)
;; (define y (stream-filter even? seq))
;; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                          seq))
;; (stream-ref y 7)
;; (display-stream z)


;; section 3.5.2
(define (integers-starting-from n)
  (cons n (delay (integers-starting-from (+ n 1)))))

(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(assert (= (stream-ref no-sevens 100) 117))


(define (fibgen a b)
  (cons a (delay (fibgen b (+ a b)))))
(define fibs (fibgen 0 1))



(define ones (cons 1 (delay ones)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons 1 (delay (add-streams ones integers))))


(define fibs
  (cons 0
        (delay (cons 1
                     (delay (add-streams (stream-cdr fibs)
                                         fibs))))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double-stream (cons 1 (delay (scale-stream double 2))))


(define (sieve stream)
  (cons
   (stream-car stream)
   (delay
     (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream))))))

(define primes (sieve (integers-starting-from 2)))

(define (square x) (* x x))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

;; (stream-ref primes 100)

;; ex: 3.53
(define s (cons 1 (delay (add-streams s s))))

;; ex: 3.54
(define (mul-streams x y)
  (stream-map * x y))
(define factorial (cons 1
                        (delay
                          (mul-streams
                           (integers-starting-from 2)
                           factorial))))

;; ex: 3.55
(define (partial-sums s)
  (cons (stream-car s)
        (delay (add-streams (partial-sums s) (stream-cdr s)))))
(define pn (partial-sums integers))

;; ex: 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons s1car (delay (merge (stream-cdr s1) s2))))
                 ((> s1car s2car)
                  (cons s2car (delay (merge s1 (stream-cdr s2)))))
                 (else
                  (cons s1car
                        (delay (merge (stream-cdr s1)
                                      (stream-cdr s2))))))))))
(define S (cons-stream 1 (merge (scale-stream integers 2)
                                (merge (scale-stream integers 3)
                                       (scale-stream integers 5)))))


;; ex: 3.58
(define (expand num den radix)
  (cons
   (quotient (* num radix) den)
   (delay (expand (remainder (* num radix) den) den radix))))
(expand 1 7 10)
(define e (expand 1 7 10))

;; ex: 3.59
;; a)
(define (integrate-series s) (stream-map / s integers))
;; b)
(define exp-series
  (cons 1 (delay (integrate-series exp-series))))
(define cosine-series
  (cons 1 (delay (stream-map - (integrate-series sine-series)))))
(define sine-series
  (cons 0 (delay (integrate-series cosine-series))))

;; ex: 3.60
(define (mul-series s1 s2)
  (cons (* (stream-car s1) (stream-car s2))
        (delay (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2)))))

(define s2+c2 (add-streams (mul-series sine-series sine-series)
                           (mul-series cosine-series cosine-series)))


;; ex: 3.61
(define (invert-unit-series s)
  (define inv
    (cons 1
          (delay (stream-map
                  -
                  (mul-series (stream-cdr s) inv)))))
  (if (not (= (stream-car s) 1))
      (error "invert series doesnt work if constent is not 1" s)
      inv))

;; ex: 3.62
(define (div-series s1 s2)
  (let ((denom-c (stream-car s2)))
    (if (= denom-c 0)
      (error "div cant work with 0 constant for div" s2)
      (let ((sn1 (scale-stream s1 (/ 1 denom-c)))
            (sn2 (scale-stream s2 (/ 1 denom-c))))
        (mul-series sn1 (invert-unit-series sn2))))))

(define tan-series (div-series sine-series cosine-series))


;; section 3.5.3
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons 1.0
          (delay (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses))))
  guesses)
;; (display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons (/ 1.0 n)
        (delay (stream-map - (pi-summands (+ n 2))))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
;; (display-stream pi-stream)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons (- s2 (/ (square (- s2 s1))
                   (+ s0 (* -2 s1) s2)))
          (delay (euler-transform (stream-cdr s))))))
;; (display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons s
        (delay (make-tableau transform
                             (transform s)))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


;; ex: 3.64
(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))
;; (define (sqrt x tolerance)
;;   (stream-limit (sqrt-stream x) tolerance))

;; ex: 3.65
(define (ln2-series n)
  (cons (/ 1.0 n)
        (delay (stream-map - (ln2-series (+ n 1))))))
(define ln2
  (partial-sums (ln2-series 1)))

(stream-limit (accelerated-sequence euler-transform ln2) 0.000001)


;; infinite stream of pairs
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons (stream-car s1)
            (delay (interleave s2 (stream-cdr s1))))))

(define (pairs s t)
  (cons
   (list (stream-car s) (stream-car t))
   (delay
     (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t))))))

(define int-pairs (pairs integers integers))

(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

;; ex: 3.67
(define (pairs-all s t)
  (cons
   (list (stream-car s) (stream-car t))
   (delay
     (interleave
      (stream-map (lambda (x) (list x (stream-car t)))
                  (stream-cdr s))
      (interleave
       (stream-map (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
       (pairs-all (stream-cdr s) (stream-cdr t)))))))


;; ex: 3.69
(define (triples s t u)
  (cons
   (list (stream-car s) (stream-car t) (stream-car u))
   (delay
     (interleave
      (stream-map (lambda (x) (cons (stream-car s)  x))
                  (pairs t (stream-cdr u)))
      (triples (stream-cdr s)
               (stream-cdr t)
               (stream-cdr u))))))

(define triple-series (triples integers integers integers))

(define pythagorean-triples
  (stream-filter (lambda (x) (= (+ (square (car x))
                                   (square (cadr x)))
                                (square (caddr x))))
                 triple-series))

;; ex: 3.70
(define (merge-weighted weight a b)
  (cond ((stream-null? a) b)
        ((stream-null? b) a)
        (else
         (let ((a-car (stream-car a))
               (b-car (stream-car b)))
           (let ((a-weight (weight a-car))
                 (b-weight (weight b-car)))
             (cond ((< b-weight a-weight)
                    (cons b-car (delay (merge-weighted weight a (stream-cdr b)))))
                   (else
                    (cons a-car
                          (delay (merge-weighted weight (stream-cdr a) b))))))))))

(define (ordered-pairs weight s t)
  (cons
   (list (stream-car s) (stream-car t))
   (delay
     (merge-weighted weight
                     (stream-map (lambda (x) (list (stream-car s) x))
                                 (stream-cdr t))
                     (ordered-pairs weight (stream-cdr s) (stream-cdr t))))))

;; a)
(define oi (ordered-pairs (lambda (a) (+ (car a) (cadr a))) integers integers))

;; b)
(define not-235-series
  (stream-filter (lambda (x) (not (or (divisible? x 2)
                                      (divisible? x 3)
                                      (divisible? x 5))))
                 integers))
(define o235
  (ordered-pairs
   (lambda (x)
     (let ((i (car x)) (j (cadr x)))
       (+ (* i 2) (* j 3) (* i j 5))))
   not-235-series not-235-series))

;; ex: 3.71
;; TODO
(define (cube x)
  (* x x x))
(define (cube-sum x)
  (+ (cube (car x)) (cube (cadr x))))

(define o-cube-sum (ordered-pairs cube-sum integers integers))

(define (con-eq s)
  (define (con-eq-internal s sum)
    (if (eq? (cube-sum (stream-car s)) sum)
        (cons (cons (stream-car s) (cube-sum (stream-car s)))
              (delay (con-eq-internal (stream-cdr s) sum)))
        (con-eq-internal (stream-cdr s) (cube-sum (stream-car s)))))
  (con-eq-internal s 0))

(define ramanujam-nums (con-eq o-cube-sum))

;; ex: 3.72
;; TODO
(define (square-sum x)
  (+ (square (car x)) (square (cadr x))))
(define o-square-sum (ordered-pairs  square-sum integers integers))
(define (square-sum-2-rep s)
  (let ((a (stream-car s))
        (b (stream-car (stream-cdr s)))
        (c (stream-car (stream-cdr (stream-cdr s)))))
    (if (and (eq? (square-sum a) (square-sum b))
             (eq? (square-sum a) (square-sum c)))
        (cons (cons a (square-sum a))
              (delay (square-sum-2-rep (stream-cdr s))))
        (square-sum-2-rep (stream-cdr s)))))

(define rama-square-sum (square-sum-2-rep o-square-sum))


;; streams and signals
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; ex: 3.73
(define (RC R C dt)
  (define (rc-fn i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C)) v0 dt)))
  rc-fn)
(define RC1 (RC 5 1 0.5))

;; ex: 3.74
(define sense-data '(1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4 0 0 0))
(define (sign-change-detector b a)
  (define (sign x) (if (< x 0) 1 0))
  (- (sign a) (sign b)))
(define (make-zero-crossings input-stream last-value)
  (cons
   (sign-change-detector (stream-car input-stream) last-value)
   (delay (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream)))))
(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings-new
  (stream-map sign-change-detector
              sense-data
              (cons 0 sense-data)))

;; ex: 3.75
(define (make-zero-crossings-avg input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons (sign-change-detector avpt last-avg)
          (delay (make-zero-crossings-avg (stream-cdr input-stream)
                                      (stream-car input-stream) avpt)))))
(define zero-crossings-avg (make-zero-crossings-avg sense-data 0 0))

;; ex: 3.76
(define (smooth s)
  (stream-map (lambda (a b) (/ (+ a b))) s (cdr s)))
(define zero-crossing-smooth
  (let ((inp (smooth sense-data)))
    (stream-map sign-change-detector
                inp
                (cons 0 inp))))

;; section 3.5.4
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons initial-value
          (delay (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int)))))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
;; (stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;; ex: 3.77
(define (integral delayed-integrand initial-value dt)
  (cons initial-value
        (delay
          (let ((integrand (force delayed-integrand)))
            (if (stream-null? integrand)
              the-empty-stream
              (integral (delay (stream-cdr integrand))
                        (+ (* dt (stream-car integrand))
                           initial-value)
                        dt))))))

;; ex: 3.78
(define (solve-2nd-non-general a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream y b) (scale-stream dy)))
  y)

;; ex: 3.79
(define (solve-2nd a b dt y0 dy0 f)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; ex: 3.80
(define (RLC R C L dt vC0 iL0)
  (define iL (integral (delay diL) iL0 dt))
  (define vC (integral (delay dvC) vC0 dt))
  (define diL (add-streams (scale-stream vC (/ 1 L))
                           (scale-stream iL (- (/ R L)))))
  (define dvC (scale-stream iL (- (/ 1 C))))
  (cons vC iL))
(define sol (RLC 1 0.2 1 0.1 0 10))

;; section 3.5.5
(define random-init 7)
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))
(define random-numbers
  (cons random-init
        (delay
          (stream-map rand-update random-numbers))))

(define (map-successive-pairs f s)
  (cons
   (f (stream-car s) (stream-car (stream-cdr s)))
   (delay (map-successive-pairs f (stream-cdr (stream-cdr s))))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons
     (/ passed (+ passed failed))
     (delay (monte-carlo
             (stream-cdr experiment-stream) passed failed))))
    (if (stream-car experiment-stream)
        (next (+ passed 1) failed)
        (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

;; ex: 3.81
;; (define (rand m)
;;   (if (eq? m 'generate)
      