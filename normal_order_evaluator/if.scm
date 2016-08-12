;; Implements syntax and semantics of object-language conditionals.

(load (list "tagged-list"
            "true-false"))

;; To evaluate a conditional, we need to know the actual value of the predicate
;; clause so that we can select the appropriate clause to evaluate next. We do not,
;; however, need to know in advance the actual values of the other two clauses.
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alernative exp) env)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false)) ; alternative not supplied

(define (if-consequent exp) (caddr exp))

(define (if-predicate exp) (cadr exp))

(define (if? exp) (tagged-list? exp 'if))
