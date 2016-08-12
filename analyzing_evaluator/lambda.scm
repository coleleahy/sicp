;; Implements syntax and semantics of object-language lambda-expressions.

(load "tagged-list")

;; When we evaluate an object-language definition '(define (f x) ... ) we bind 'f to
;; the value of an analyzed lambda-expression. Then, when we evaluate an object-
;; language combination '(f <term>) we apply the *analyzed* body of our analyzed
;; lambda-expression to our current environment extended with a binding of 'x to the
;; value of '<term>. This yields a major gain in efficiency, since we would otherwise
;; have to plod through the syntactic analysis of the lambda's body each time we
;; wanted to evaluate a combination in which 'f is the operator.
(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp)) ; extract once and for all
        (body-proc (analyze-sequence (lambda-body exp)))) ; analyze once and for all
    (lambda (env) (make-procedure params body-proc env))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda-body exp) (cddr exp))

(define (lambda-parameters exp) (cadr exp))

(define (lambda? exp) (tagged-list? exp 'lambda))
