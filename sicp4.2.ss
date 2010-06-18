(require errortrace)

(load "ch4-mceval.scm")

;; the lazy eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; lazy apply with few changes
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (remove-delay-tags (procedure-parameters procedure)) ;changed
           (list-of-args arguments                              ;changed
                         (procedure-parameters procedure)
                         env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (remove-delay-tags vars)
  (map (lambda (x) (if (lazy-arg? x) (lazy-var x) x)) vars))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (lazy-var var)
  (and (pair? var) (car var)))
(define (lazy-only-arg? arg)
  (and (pair? arg) (eq? (cadr arg) 'lazy)))
(define (lazy-memo-arg? arg)
  (and (pair? arg) (eq? (cadr arg) 'lazy-memo)))
(define (lazy-arg? arg)
  (dbg "larzy-arg?" arg (or (lazy-only-arg? arg)
                            (lazy-memo-arg? arg)))
  (or (lazy-only-arg? arg)
      (lazy-memo-arg? arg)))

(define (list-of-args vals vars env)
  (dbg "list-of-args" vars vals)
  (if (or (null? vars) (null? vals))
      '()
      (cons (cond ((lazy-only-arg? (car vars)) (delay-it (car vals) env))
                  ((lazy-memo-arg? (car vars)) (delay-it-memo (car vals) env))
                  (else (actual-value (car vals) env)))
            (list-of-args (cdr vars) (cdr vals) env))))

(define (thunk? obj)
  (cond ((mpair? obj) (or (eq? (mcar obj) 'lazy-only) (eq? (mcar obj) 'lazy-memo)))
        ((pair? obj) (or (eq? (car obj) 'lazy-only) (eq? (car obj) 'lazy-memo)))
        (else #f)))

(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (cond ((mpair? obj) (eq? (mcar obj) 'evaluated-thunk))
        (else #f)))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;; delay it should take care of tagging the arg exp based on type of lazyness.
(define (delay-it exp env)
  (dbg "delay-it" exp)
  (mlist 'lazy-only exp env))
(define (delay-it-memo exp env)
  (dbg "delay-it-memo" exp)
  (mlist 'lazy-memo exp env))

(define (force-it obj)
  (define (eval-value obj) (actual-value (thunk-exp obj) (thunk-env obj)))
  (define (set-value! obj val)
    (set-mcar! obj 'evaluated-thunk)
    (set-mcar! (mcdr obj) val)
    (set-mcdr! (mcdr obj) '())
    val)

  (cond ((thunk? obj)
         (let ((res (eval-value obj)))
           (if (eq? (mcar obj) 'lazy-memo)
               (set-value! obj res)
               res)))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


(eval '((lambda ((a lazy)) (a 1)) (lambda (x) (display 5) (newline) 2)) the-global-environment)


(define input-prompt "- leval:")
(define output-prompt "- leval out:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


'EX_4_31