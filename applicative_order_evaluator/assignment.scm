;; Implements syntax and semantics of object-language assignments.

(load (list "environments"
            "tagged-list"))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (make-assignment var val) (list 'set! var val))

(define (assignment-value exp) (caddr exp))

(define (assignment-variable exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
