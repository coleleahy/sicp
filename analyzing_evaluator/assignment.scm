;; Implements syntax and semantics of object-language assignments.

(load (list "environments"
            "tagged-list"))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp)) ; extract once and for all
        (proc (analyze (assignment-value exp)))) ; analyze once and for all
    (lambda (env) (set-variable-value! var (proc env) env)
            'ok)))

(define (make-assignment var val) (list 'set! var val))

(define (assignment-value exp) (caddr exp))

(define (assignment-variable exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
