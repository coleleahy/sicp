;; Implements syntax and semantics of object-language definitions.

(load (list "environments"
            "lambda"
            "tagged-list"))

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp) ; for (define <var> <val>)
      (make-lambda (cdadr exp) ; for (define (<var> <p1> ... <pn>) <body>)
                   (cddr exp))))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) ; for (define <var> <val>)
      (caadr exp))) ; for (define (<var> <p1> ... <pn>) <body>)

(define (definition? exp) (tagged-list? exp 'define))
