(load "debug.ss")

(define *show-debug* #t)

;; ex: 3.1
(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ acc x))
    acc))

;; ex: 3.2
(define (make-monitored f)
  (let ((call-count 0))
    (define (inc)
      (set! call-count (+ call-count 1)))
    (define (rst)
      (set! call-count 0))
    (define (cur) call-count)
    (lambda (x)
      (cond ((eq? x 'how_many_calls?) (cur))
            ((eq? x 'reset-count) (rst))
            (else (inc) (f x))))))

;; ex: 3.3 and 3.4
(define (make-account balance pswd)
  (let ((inval-attempts 0))
    (define (call-the-cops)
      (display "calling the cops"))
    (define (handle-inval-pswd)
      (set! inval-attempts (+ inval-attempts 1))
      (if (< inval-attempts 8)
          (lambda (x) (display "incorrect password"))
          (call-the-cops)))
    
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch inp-pswd m)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    (lambda (inp-spwd m)
      (if (eq? inp-pswd pswd)
          (dispatch inp-pswd m)
          (handle-inval-pswd)))))


;; ex: 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 y1 x2 y2)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo 10000 experiment) (* (* (- x2 x1) (- y2 y1)) 1.0)))


;; value of pi!
;; (estimate-integral (lambda (x y) (< (+ (sqr x ) (sqr y)) 2))
;;                    -1 -1 1 1)
;; (estimate-integral (lambda (point) (< (+ (sqr (- (car point) 5)) (sqr (- (cdr point) 7))) 10))
;;                    2 4 8 10)


;; ex: 3.6
(define (rand m)
  (cond ((eq? m 'reset) (lambda (x) (random-seed x)))
        ((eq? m 'generate) (lambda () (random)))
        (else (display "something wrong"))))

;; ex: 3.7
(define (make-password-check pswd)
  (let ((inval-attempts 0))
    (define (call-the-cops)
      (display "calling the cops"))
    (define (handle-inval-pswd)
      (set! inval-attempts (+ inval-attempts 1))
      (if (< inval-attempts 8)
          (begin (display "incorrect password")(newline))
          (call-the-cops)))
    (lambda (inp-pswd fn)
      (if (eq? inp-pswd pswd)
          (fn)
          (handle-inval-pswd)))))

(define (make-account balance pswd)
  (let ((check-password-and-exec? (make-password-check pswd)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'account) dispatch)
                (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    (lambda (inp-pswd m)
      (check-password-and-exec? inp-pswd (lambda () (dispatch m))))))

(define (make-joint acc pswd new-pswd)
  (let ((dispatch null)
        (check-password-and-exec? (make-password-check new-pswd)))

    (set! dispatch (acc pswd 'account))
    (lambda (inp-pswd m)
      (check-password-and-exec? inp-pswd (lambda () (dispatch m))))))

;; (define acc (make-account 100 'old))
;; ((acc 'old 'deposit) 10)
;; ((acc 'ra 'deposit) 15)

;; (define ja (make-joint acc 'old 'new))
;; ((ja 'old 'deposit) 30)
;; ((ja 'new 'deposit) 40)
;; ((ja 'new 'withdraw) 10)

;; ex: 3.8
(define f
  (let ((last-value 0))
    (lambda (x)
      (if (= x 0)
          (set! last-value (+ (- last-value) 1))
          (set! last-value (- last-value)))
      last-value)))


