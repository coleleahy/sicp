;; Constructors and selectors for frames and the bindings contained therein

;; Attempts to instantiate exp using the variable bindings found in frame, calling
;; unbound-var-handler if frame lacks necessary bindings.
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (binding-value binding)
  (cdr binding))

(define (binding-variable binding)
  (car binding))

(define (make-binding variable value)
  (cons variable value))
