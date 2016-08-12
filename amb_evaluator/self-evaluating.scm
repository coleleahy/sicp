;; Implements the syntax (and therefore semantics) of self-evaluating expressions.

;; Required by: eval.scm

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
