;; Implements syntax and semantics of object-language lambda-expressions.

(load "tagged-list")

(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp))
        (body-proc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure params body-proc env)
               fail))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda-body exp) (cddr exp))

(define (lambda-parameters exp) (cadr exp))

(define (lambda? exp) (tagged-list? exp 'lambda))
