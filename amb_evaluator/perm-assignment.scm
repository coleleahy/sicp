(define (analyze-perm-assignment exp)
  (let ((var (perm-assignment-variable exp))
        (val-proc (analyze (perm-assignment-value exp))))
    (lambda (env succeed fail)
      (val-proc env
                (lambda (val fail2)
                  (set-variable-value! var val env)
                  (succeed 'ok fail2))
                (fail)))))

(define (make-perm-assignment var val) (list 'permanent-set! var val))

(define (perm-assignment-value exp) (caddr exp))

(define (perm-assignment-variable exp) (cadr exp))

(define (perm-assignment? exp) (tagged-list? exp 'permanent-set!))
