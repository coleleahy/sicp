;; Implements syntax and semantics of object-language variables.

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (variable? exp) (symbol? exp))
