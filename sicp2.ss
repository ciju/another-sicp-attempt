(define (average a b)
  (/ (+ a b) 2))

; GCD
;(define (gcd a b)
;  (if (= b 0)
;      a
;      (gcd b (remainder a b))))

; rational numbers
; (define (make-rat n d) (cons n d))
;(define (make-rat n d) 
;  (let ((g (gcd n d)))
;    (cons (/ n g) (/ d g))))
; ex: 2.1
(define (make-rat n d)
  (cond ((< d 0)
         (make-rat (- n) (- d)))
        (else (let ((g (gcd n d)))
           (cons (/ n g) (/ d g))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


; ex: 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (mid-point ps pe)
  (make-point (average (x-point ps) (x-point pe))
              (average (y-point ps) (y-point pe))))

(define (make-segment p1 p2)
  (list p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (car (cdr s)))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (mid-point (start-segment s) (end-segment s)))
  
(define (mcons x y)
  (lambda (m) (m x y)))
(define (mcar z)
  (z (lambda (p q) p)))
; ex: 2.4
(define (mcdr z)
  (z (lambda (p q) q)))

; section 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
; ex: 2.7
(define (make-interval a b) (cons a b))
(define (comf-interval c i)
  (cond ((c (car i) (cdr i)) (cdr i))
        (else (car i))))
(define (upper-bound i) 
  (comf-interval < i))
(define (lower-bound i)
  (comf-interval > i))
; ex: 2.8
(define sub-interval (x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))
