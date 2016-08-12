;; Implements syntax and semantics of object-language conditionals.

(load (list "tagged-list"
            "true-false"))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false)) ; alternative not supplied

(define (if-consequent exp) (caddr exp))

(define (if-predicate exp) (cadr exp))

(define (if? exp) (tagged-list? exp 'if))
