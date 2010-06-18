(define (error msg list)
  (display msg)
  (display "   ")
  (display list)
  (newline))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; from section 3.3.3 for tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; normal scheme number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

;complex numbers

; sub modules: rectangular 
(define (install-rectangular-package)
  ;; internal procedures
  (define (r-part z) (car z))
  (define (i-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magn z)
    (sqrt (+ (square (r-part z))
             (square (i-part z)))))
  (define (angl z)
    (atan (i-part z) (r-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'r-part '(rectangular) r-part)
  (put 'i-part '(rectangular) i-part)
  (put 'magn '(rectangular) magn)
  (put 'angl '(rectangular) angl)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; submodule: polar
(define (install-polar-package)
  ;; internal procedures
  (define (magn z) (car z))
  (define (angl z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (r-part z)
    (* (magn z) (cos (angl z))))
  (define (i-part z)
    (* (magn z) (sin (angl z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'r-part '(polar) r-part)
  (put 'i-part '(polar) i-part)
  (put 'magn '(polar) magn)
  (put 'angl '(polar) angl)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (r-part z) (apply-generic 'r-part z))
(define (i-part z) (apply-generic 'i-part z))
(define (magn z) (apply-generic 'magn z))
(define (angl z) (apply-generic 'angl z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (r-part z1) (r-part z2))
                         (+ (i-part z1) (i-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (r-part z1) (r-part z2))
                         (- (i-part z1) (i-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magn z1) (magn z2))
                       (+ (angl z1) (angl z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magn z1) (magn z2))
                       (- (angl z1) (angl z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(put 'r-part '(complex) r-part)
(put 'i-part '(complex) i-part)
(put 'magn '(complex) magn)
(put 'angl '(complex) angl)

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;convenience
(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))

;ex: 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))


;ex: 2.79
(define (install-equ?)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ; better be defined in rational package with accessors, or 'put accessors in rational package
  (put 'equ? '(rational rational)
       (lambda (x y) (and (equ? (numer x) (numer y)) (equ? (denom x) (denom y)))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (equ? (r-part x) (r-part y)) (equ? (i-part x) (i-part y)))))
  'done)
(install-equ?)
(define (equ? x y)
  (apply-generic 'equ? x y))

;(equ? (make-rational 4 2) (make-rational 2 1))
;(equ? (make-complex-from-real-imag 4 2) (sub (make-complex-from-real-imag 5 3) (make-complex-from-real-imag 1 1)))

;ex: 2.80
(define (install-=zero?)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put '=zero? '(rational)
       (lambda (x) (= (car x) 0)))
  (put '=zero? '(complex)
       (lambda (x) (and (= (i-part x) 0) (= (r-part x) 0))))
  'installed-zero)
(install-=zero?)
(define (=zero? x)
  (apply-generic '=zero? x))

;(=zero? (sub (make-rational 4 4) (make-rational 1 1)))

; section 2.5.2

; coercion table
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'rational 'complex 
               (lambda (r) (make-complex-from-real-imag (/ (numer r) (denom r)) 0)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types" 
                           (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

;ex: 2.81
;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(define (rational->rational r) r)
;(put-coercion 'scheme-number 'scheme-number
;              scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)
;(put-coercion 'rational 'rational rational->rational)

(define (mexp x y) (apply-generic 'exp x y))
;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (attach-tag 'scheme-number (expt x y)))) ; using primitive expt

;ex: 2.82
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((co-args (apply-coercion-strategy args)))
            (apply (get op (map type-tag co-args)) co-args))))))

(define (apply-coercion-strategy args)
  (define (get-coercions t args)
    (if (null? args) 
        '()
        (let ((from-type (type-tag (car args))))
          (let ((this (if (eq? t from-type)
                          (lambda (x) x)
                          (get-coercion from-type t))))
            (if this
                (let ((rest (get-coercions t (cdr args))))
                  (if rest
                      (cons (lambda (x) (this (car args))) rest)
                      #f))
                #f)))))
  (define (try-coercions rest)
    (if (null? rest) 
        #f
        (let ((coercions (get-coercions (type-tag (car rest)) args)))
          (if coercions
              (map (lambda (x) (x (contents (car rest)))) coercions)
              (try-coercions (cdr rest))))))
  (try-coercions args))

;ex: 2.83
(define (install-raise)
  (put-coercion 'raise 'scheme-number
       (lambda (x) (error "raise scheme-number" x) (make-rational x 1)))
  (put-coercion 'raise 'rational
;       (lambda (x) (attach-tag 'real (/ (numer x) (denom x)))))
;  (put 'raise '(real)
       (lambda (x) (error "raise rational" x) (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
  'installed-raise)
(install-raise)

;ex: 2.84
(define (apply-generic op . args)
  ; asumes (= (length args) 2)
  (define (raise-till t a)
    (let ((raise-fn (get-coercion 'raise (type-tag a))))
      (let ((raised (if raise-fn (raise-fn a) #f)))
        (cond (raise-fn
               (if (eq? (type-tag t) (type-tag raised))
               raised
               (raise-till t raised)))
              (else #f)))))
  (define (coerce-and-apply args)
    (let ((t1 (raise-till (car args) (cadr args)))
          (t2 (raise-till (cadr args) (car args))))
      (cond (t1 (apply-generic op (car args) t1))
            (t2 (apply-generic op t2 (cadr args)))
            (display "something wrong"))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (coerce-and-apply args)))))

;ex: 2.85
(define (install-project)
  (put-coercion 'project 'complex
                (lambda (x) (error "project complex" x) (make-rational (round (r-part x)) 1)))
  (put-coercion 'project 'rational (lambda (x) (error "project rational" x) (numer x)))
  'installed-project)
(install-project)

(define (drop a)
  (define (cond-apply f a) (if f (f a) f))
  (let ((dropped (cond-apply (get-coercion 'project (type-tag a)) a)))
    (display "a: ")(display a)(newline)
    (if dropped
        (let ((raised-back (get-coercion 'raise (type-tag dropped))))
          (display dropped)(display raised-back)(newline)
          (if (equ? a (raised-back dropped))
              (drop dropped)
              a))
        a)))
  
(define (apply-generic op . args)
  (define (apply-generic-internal op args)
  ; asumes (= (length args) 2)
    (define (raise-till t a)
      (let ((raise-fn (get-coercion 'raise (type-tag a))))
        (if raise-fn
            (let ((raised (raise-fn a)))
              (if (eq? (type-tag t) (type-tag raised))
                  raised
                  (raise-till t raised)))
            #f)))
    (define (coerce-and-apply args)
      (let ((t1 (raise-till (car args) (cadr args)))
            (t2 (raise-till (cadr args) (car args))))
        (error "coercing: " (list t1 t2))
        (cond (t1 (apply-generic-internal op (list (car args) t1)))
              (t2 (apply-generic-internal op (list t2 (cadr args))))
              (display "something wrong"))))
    ;  (display op) (display args)(newline)
    (error "applying" op)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (error "still applying" (list "tags:" type-tags "proc" proc))
        (if proc
            (apply proc (map contents args))
            (coerce-and-apply args)))))
  ;  (apply-generic-internal op args))
  (drop (apply-generic-internal op args)))
;  (let ((res (apply-generic-internal op args)))
;    (display res)(newline)
;    (let ((dropped (drop res)))
;      (if dropped dropped res))))
