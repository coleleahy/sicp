;; Implements syntax and semantics of object-language variables.

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (variable? exp) (symbol? exp))
