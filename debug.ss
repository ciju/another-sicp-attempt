;; (require rnrs/base-6)
;; (require (lib "trace.ss"))


(define (dbg msg . args)
  (cond ((not *show-debug*) #f)
        (else (display msg)
              (map (lambda (x) (display " ")(display x)) args)
              (newline))))

(define (error msg list)
  (display msg)
  (display "   ")
  (display list)
  (newline))

