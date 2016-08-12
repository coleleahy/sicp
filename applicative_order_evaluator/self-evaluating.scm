;; Implements the syntax (and therefore semantics) of self-evaluating expressions.

;; Required by: eval.scm

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
