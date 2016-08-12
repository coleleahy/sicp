;; Implements syntax and semantics of object-language conditionals.

(load (list "tagged-list"
            "true-false"))

(define (analyze-if exp)
  (let ((prd (analyze (if-predicate exp)))
        (csq (analyze (if-consequent exp)))
        (alt (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (prd env
           (lambda (prd-value fail2) ; continuation if value found for prd
             (if (true? prd-value)
                 (csq env succeed fail2)
                 (alt env succeed fail2)))
           fail)))) ; continuation if dead end reached for prd

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false)) ; alternative not supplied

(define (if-consequent exp) (caddr exp))

(define (if-predicate exp) (cadr exp))

(define (if? exp) (tagged-list? exp 'if))
