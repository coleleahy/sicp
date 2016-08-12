;; Implements syntax and semantics of object-language conditionals.

(load (list "tagged-list"
            "true-false"))

(define (analyze-if exp)
  (let ((prd (analyze (if-predicate exp))) ; analyze once and for all
        (csq (analyze (if-consequent exp))) ; analyze once and for all
        (alt (analyze (if-alternative exp)))) ; analyze once and for all
    (lambda (env) (if (true? (prd env))
                      (csq env)
                      (alt env)))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false)) ; alternative not supplied

(define (if-consequent exp) (caddr exp))

(define (if-predicate exp) (cadr exp))

(define (if? exp) (tagged-list? exp 'if))
