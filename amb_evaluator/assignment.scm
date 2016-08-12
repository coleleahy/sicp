;; Implements syntax and semantics of object-language assignments.

(load (list "environments"
            "tagged-list"))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (val-proc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (val-proc env
                (lambda (val fail2) ; continuation if value found for val-proc
                  (let ((old-value
                         (lookup-variable-value var env)))
                    (set-variable-value! var val env)
                    (succeed 'ok
                             (lambda () ; in case we need to back out of the assignment
                               (set-variable-value! var
                                                    old-value
                                                    env)
                               (fail2)))))
                (fail))))) ; continuation if dead end reached for val-proc

(define (make-assignment var val) (list 'set! var val))

(define (assignment-value exp) (caddr exp))

(define (assignment-variable exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
