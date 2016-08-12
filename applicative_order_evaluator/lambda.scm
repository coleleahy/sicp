;; Implements syntax and semantics of object-language lambda-expressions.

(load "tagged-list")

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda-body exp) (cddr exp))

(define (lambda-parameters exp) (cadr exp))

(define (lambda? exp) (tagged-list? exp 'lambda))
