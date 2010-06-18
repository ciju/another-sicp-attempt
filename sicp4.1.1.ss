(load "debug.ss")
(require scheme/mpair)
;; (load "ch4-mceval.scm")
(define *show-debug* #t)

(define apply-in-underlying-scheme apply)

;; ex: 4.3

(define (make-table)
  (let ((local-table (make-hash)))
    (define (lookup key-1)
      (hash-ref local-table key-1 #f))
    (define (insert! key-1 value)
      (hash-set! local-table key-1 value)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((op (get (operator exp))))
           (cond (op (op exp env))
                 ((application? exp)
                  (apply (eval (operator exp) env)
                         (list-of-values (operands exp) env)))
                 (else
                  (error "unknown expression type" exp)))))))
;; ex: 4.3 end


;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type -- EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


;; section 4.1.2 representing expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;; assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; definition
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body


;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;; conditions
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; section 4.1.3
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable - lookup old" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; section 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '= =)
        (list '- -)
        (list 'list list)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
;; (define the-global-environment (setup-environment))
;; (driver-loop)

'METACIRCULAR-EVALUATOR-LOADED

;; ex: 4.1
;; left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-exp-val (eval (first-operand exps) env)))
        (cons first-exp-val
              (list-of-values (rest-operands exps) env)))))

;; right to left
(define (list-of-values-right exps env)
  (if (no-operands? exps)
      '()
      ((lambda (rest-exps-vals)
         (cons (eval (first-operand exps) env)
               rest-exps-vals))
       (list-of-values-right (rest-operands exps) env))))


;; ex: 4.3

(define (make-table)
  (let ((local-table (make-hash)))
    (define (lookup key-1)
      (hash-ref local-table key-1 #f))
    (define (insert! key-1 value)
      (hash-set! local-table key-1 value)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'quote (lambda (exp env) (cadr exp)))
(put 'set! eval-assignment)
(put 'define eval-definition)
(put 'if eval-if)
(put 'lambda (lambda (exp env)
               (make-procedure (lambda-parameters exp)
                               (lambda-body exp)
                               env)))
(put 'begin (lambda (exp env)
              (eval-sequence (begin-actions exp) env)))
;; (put 'cond (lambda (exp env)
;;              (eval (cond->if exp) env)))
     

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((op (get (operator exp))))
           (cond (op (op exp env))
                 ((application? exp)
                  (apply (eval (operator exp) env)
                         (list-of-values (operands exp) env)))
                 (else
                  (error "unknown expression type" exp)))))))

;; ex: 4.4
(define (eval-and exps env)
  (define (eval-and-int exps)
    (if (null? exps)
        'true
        (let ((first-eval (eval (first-exp exps) env)))
          (cond ((last-exp? exps) first-eval)
                ((true? first-eval) (eval-and-int (rest-exps exps)))
                (else 'false)))))
  (eval-and-int (operands exps)))
(define (eval-or exps env)
  (define (eval-or-int exps)
    (if (null? exps)
        'false
        (let ((first-eval (eval (first-exp exps) env)))
          (cond ((last-exp? exps) first-eval)
                ((not (true? first-eval)) (eval-or-int (rest-exps exps)))
                (else first-eval)))))
  (eval-or-int (operands exps)))

(put 'and eval-and)
(put 'or eval-or)


;; ex: 4.5
;; cond
(define (cond-exp p l)
  (if (eq? (car l) '=>)
      (list (cadr l) p)
      (sequence->exp l)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (cond-exp (cond-predicate first) (cond-actions first))
                     ;; (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(put 'if eval-if)
(put 'cond (lambda (exp env)
             (eval (cond->if exp) env)))

;; ex: 4.6
(define (let-body exp)
  (cddr exp))
(define (let-assoc exp)
  (cadr exp))
(define (split-pairs lis)
  (foldr (lambda (x l) (cons (cons (car x) (car l)) (cons (cadr x) (cdr l))))
         (cons '() '())
         (mlist->list lis)))
(define (let->combination exp)
  (let ((exps (cdr (split-pairs (let-assoc exp))))
        (vals (car (split-pairs (let-assoc exp)))))
    (cons (make-lambda vals (let-body exp))
          exps)))
(put 'let (lambda (exp env)
            (eval (let->combination exp) env)))

(define global-env (setup-environment))

;; ex: 4.7
(define (make-let var val body)
  (list 'let (list (list var val)) body))
(define (let*-body exp)
  (caddr exp))
(define (let*-assocs exp)
  (cadr exp))

(define (let*->nested-lets exp)
  (define (let*->nested-rec assocs body)
    (if (null? assocs)
        body
        (make-let (caar assocs) (cadar assocs) (let*->nested-rec (cdr assocs) body))))
  (let*->nested-rec (let*-assocs exp) (let*-body exp)))
(put 'let* (lambda (exp env)
             (eval (let*->nested-lets exp) env)))

;; ex: 4.8
(define (let-body exp)
  (cddr exp))
(define (let-assoc exp)
  (cadr exp))
(define (named-let-name exp)
  (cadr exp))
(define (named-let-body exp)
  (cadddr exp))
(define (named-let-assoc exp)
  (caddr exp))
(define (named-let? exp)
  (not (pair? (cadr exp))))
  ;; (= (length exp) 4))
(define (split-pairs lis)
  (foldr (lambda (x l) (cons (cons (car x) (car l)) (cons (cadr x) (cdr l))))
         (cons '() '())
         (mlist->list lis)))
(define (let->combination exp)
  (if (named-let? exp)
      (let ((exps (cdr (split-pairs (named-let-assoc exp))))
            (vals (car (split-pairs (named-let-assoc exp)))))
        (sequence->exp
         (list
          (list 'define (cons (named-let-name exp) vals)
                (named-let-body exp))
          (cons (named-let-name exp) exps))))
      
      (let ((exps (cdr (split-pairs (let-assoc exp))))
            (vals (car (split-pairs (let-assoc exp)))))
        (cons (make-lambda vals (let-body exp))
              exps))))
(put 'let (lambda (exp env)
            (eval (let->combination exp) env)))
;; (eval '(define (fib n) (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) global-env)
;; (eval '(fib 10) global-env)


;; ex: 4.11
;; replace these with the originals
;; (define (make-frame vars vals)
;;   (map cons vars vals))
;; (define (frame-variables frame) (map car frame))
;; (define (frame-values frame) (map cdr frame))
;; (define (add-binding-to-frame! var val frame)
;;   (dbg "added binding: " var val)
;;   (set-mcdr! frame (mcons (mcar frame) (mcdr frame)))
;;   (set-mcar! frame (cons var val)))

;; ex: 4.12
(define (frame-scan frame var found-action null-action)
  (define (frame-scan-rec vars vals)
    (cond ((null? vars)
           (null-action))
          ((eq? var (mcar vars))
           (found-action vars vals))
          (else (frame-scan-rec (mcdr vars) (mcdr vals)))))
  (frame-scan-rec (frame-variables frame)
                  (frame-values frame)))

(define (act-on-env var found-action env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "unbound variable - frame" var)
        (frame-scan
         (first-frame env)
         var
         found-action
         (lambda () (env-loop (enclosing-environment env))))))
  (env-loop env))
  
    
(define (lookup-variable-value var env)
  (act-on-env var
              (lambda (vars vals) (mcar vals))
              env))
(define (set-variable-value! var val env)
  (act-on-env var
              (lambda (vars vals) (set-mcar! vals val))
              env))

(define (define-variable! var val env)
  (frame-scan (first-frame env)
              var
              (lambda (vars vals)
                (set-mcar! vals val))
              (lambda ()
                (add-binding-to-frame! var val (first-frame env)))))
   
(define (make-unbound! var env)
  (frame-scan (first-frame env)
              var
              (lambda (vars vals)
                (set-mcar! vals '()) (set-mcar! vars '()))
              (lambda ()
                (error "unbound variable -  make-unbound" env))))

;; ex: 4.16
(define (lookup-variable-value var env)
  (act-on-env var
              (lambda (vars vals) (if (eq? (mcar vals) '*unassigned*)
                                      (error "unassigned variable - lookup" var)
                                      (mcar vals)))
              env))

(define (scan-out-defines exp)
  (define (get-define-vars exp)
    (cond ((null? exp) '())
          ((definition? (car exp))
           (cons (list (definition-variable (car exp)) '(quote *unassigned*))
                 (get-define-vars (cdr exp))))
          (else (get-define-vars (cdr exp)))))
  (define (process-local-defines exp)
    (if (definition? exp)
        (cons 'set! (cdr exp))
        exp))
  (if (not (null? (get-define-vars exp)))
      (begin 
        (list (cons 'let (cons (get-define-vars exp) (map process-local-defines exp))))
        )
      exp))
  
  
(define (make-procedure parameters body env)
  ;; (dbg "prov: " (scan-out-defines body))
  (list 'procedure parameters (scan-out-defines body) env))

;; (define ex '(define (fn) (define a (lambda (x) (+ x 1))) (+ (a 2) 3)))
;; (eval ex global-env)
;; (eval '(fn) global-env)

;; (define ex1 '(define (f x)
;;                (define even?
;;                  (lambda (n)
;;                    (if (= n 0)
;;                        true
;;                        (odd? (- n 1)))))
;;                (define odd?
;;                  (lambda (n)
;;                    (if (= n 0)
;;                        false
;;                        (even? (- n 1)))))
;;                (even? x)))
;; (eval ex1 global-env)
;; (eval '(f 5) global-env)

;; ex: 4.20
(define (letrec-body exp)
  (cddr exp))
(define (letrec-assocs exp)
  (cadr exp))

(define (letrec->lets exp)
  (define (letrec->lets-assocs assoc)
    (map (lambda (x)
           (list (car x) '(quote *unassigned*))) assoc))
  (define (letrec->lets-assign assoc)
    (map (lambda (x)
           (cons 'set! x)) assoc))
  (cons 'let
        (cons
         (letrec->lets-assocs (letrec-assocs exp))
         (append
          (letrec->lets-assign (letrec-assocs exp))
          (letrec-body exp)))))
(put 'letrec (lambda (exp env)
               (eval (letrec->lets exp) env)))

;; ex: 4.21
((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fib n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib fib (- n 1)) (fib fib (- n 2))))))))
 10)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
(f 11)


;; ex: 4.22
(define (analyze-let exp)
  (analyze (let->combination exp)))