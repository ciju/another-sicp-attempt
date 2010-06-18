(require errortrace)

(load "ch4-query.scm")

;; execute in the loop
;; ex: 4.55
(supervisor ?x (Bitdiddle Ben))
(job ?x (accounting . ?y))
(address ?x (Slumerville . ?y))

;; ex: 4.56
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?address))
(and (salary ?person ?amount)
     (salary (Bitdiddle Ben) ?bens-salary)
     (lisp-value < ?amount ?bens-salary))
(and (supervisor ?person ?supervisor)
     (not
      (job ?supervisor (computer . ?any))))
;; ex: 4.57
(rule (can-replace ?p1 ?p2)
      (and 
       (or (and (job ?p1 ?j)
                (job ?p2 ?j))
           (and (job ?p1 ?j1)
                (job ?p2 ?j2)
                (can-do-job ?j1 ?j2)))
       (not (same ?p1 ?p2))))

(and (can-replace ?a ?b)
     (salary ?a ?sa)
     (salary ?b ?sb)
     (lisp-value > ?sb ?sa))

;; ex: 4.58
(rule (bigshot ?p ?div)
      (and (job ?p (?div . ?j))
           (supervisor ?p ?boss)
           (not (job ?boss (?div . ?any))))) 


;; ex: 4.59
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
           (and
            (job ?person (?div . ?sub-div))
            (meeting ?div ?day-and-time))))

(and (meeting-time (Hacker Alyssa P)
                   (Wednesday ?time))
     (meeting ?div (Wednesday ?time)))

;; ex: 4.62
(rule (last-pair (?x) ?x))
(rule (last-pair (?a . ?b) ?x)
      (last-pair ?b ?x))
))

;; ex: 4.63
(rule (grandson ?g ?s)
      (and (son ?g ?m)
           (son ?m ?s)))
(rule (son ?m ?s)
      (and (wife ?m ?w)
           (son ?w ?s)))

;; ex: 4.68
(rule (reverse () ()))
(rule (reverse ?lst ?rev)
      (and
       (append-to-form (?u) ?y ?lst)
       (reverse ?y ?y-rev)
       (append-to-form ?y-rev (?u) ?rev)))

;; ex: 4.69
(rule ((grandson) ?x ?y)
      (grandson ?x ?y))
(rule ((great . ?rel) ?x ?y)
      (and
       (append-to-form ?head (grandson) ?rel)
       (?rel ?sx ?y)
       (son ?x ?sx)))
