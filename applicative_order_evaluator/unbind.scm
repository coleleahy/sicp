;; Implements syntax and semantics of object-language un-assignments. The given
;; variable is unbound only in the first frame where it is bound, since unbinding
;; in subsequent frames would increase the risk that we break an environment
;; extending from that frame; there is already enough risk that we will break an
;; environment extending from the first frame where the variable is bound.

(load (list "environments"
            "tagged-list"))

(define (eval-unbind exp env)
  (let ((var (unbind-variable exp))
        (frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (error "Unbound variable -- UNBIND!" var))
            ((eq? var (car vars))
             (set! vars (cdr vars))
             (set! vals (cdr vals)))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (unbind-variable exp) (cadr exp))

(define (unbind? exp) (tagged-list? exp 'unbind!))
