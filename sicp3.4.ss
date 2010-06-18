(load "debug.ss")

;; (require scheme/mpair)

(define *show-debug* #t)

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

;; ex: 3.47
;; a)
(define (make-semaphore-mutex n)
  (let ((count n) (mutex (make-mutex)))
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (if (mutex 'acquire)
             