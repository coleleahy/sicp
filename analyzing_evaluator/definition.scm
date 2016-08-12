;; Implements syntax and semantics of object-language definitions.

(load (list "environments"
            "lambda"
            "tagged-list"))

(define (analyze-definition exp)
  (let ((var (definition-variable exp)) ; extract once and for all
        (val-proc (analyze (definition-value exp)))) ; analyze once and for all
    (lambda (env) (define-variable! var (val-proc env) env)
            'ok)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp) ; for (define <var> <val>)
      (make-lambda (cdadr exp) ; for (define (<var> <p1> ... <pn>) <body>)
                   (scan-out-defines (cddr exp)))))

;; If proc-body contains any definitions, convert proc-body to a let-expression
;; in which the defined variables are initially bound to a dummy value, and only
;; later set to the desired values. The effect is that the definitions internal
;; to proc-body are "simultaneous" in the sense that the definitions have the
;; same scope, namely the whole of proc-body. This simultaneous-scoping rule
;; stands in contrast to the sequential-scoping rule according to which the
;; scope of an internal definition would be the portion of proc-body beginning
;; with the given definition, excluding earlier parts of proc-body.
(define (scan-out-defines proc-body)
  (define (lacks-definitions? body)
    (let* ((compound-exps
            (filter list? body))
           (definitions
            (filter (lambda (x) (eq? 'define (car x)))
                    compound-exps)))
      (null? definitions)))
  (define (iter old-body new-bindings new-body)
    (if (null? old-body)
        (list new-bindings new-body)
        (let ((first-exp (first-exp old-body))
              (rest-exps (rest-exps old-body)))
          (cond ((definition? first-exp)
                 (let* ((the-variable
                         (definition-variable first-exp))
                        (the-binding
                         (list the-variable (make-quoted '*unassigned*)))
                        (the-value
                         (definition-value first-exp))
                        (the-assignment
                         (make-assignment the-variable the-value)))
                   (iter rest-exps
                         (append new-bindings (list the-binding))
                         (append new-body (list the-assignment)))))
                (else (iter rest-exps
                            new-bindings
                            (append new-body (list first-exp))))))))
  (if (lacks-definitions? proc-body)
      proc-body
      (let* ((let-bindings-and-body
              (iter proc-body '() '()))
             (let-bindings
              (car let-bindings-and-body))
             (let-body
              (cadr let-bindings-and-body)))
        (list (make-let let-bindings let-body)))))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) ; for (define <var> <val>)
      (caadr exp))) ; for (define (<var> <p1> ... <pn>) <body>)

(define (definition? exp) (tagged-list? exp 'define))
