;; Implements environment structures to support semantics of object-language

(load "primitive-procedures")

;; Creates a new environment, extending the empty environment, in which the
;; object language's primitive-procedure names etc. are bound to appropriate
;; values.
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (null-action env)
    (env-loop var (enclosing-environment env) null-action eq-action))
  (define (eq-action vals)
    (if (eq? '*unassigned* (car vals))
        (error "Unassigned variable -- LOOKUP" var)
        (car vals)))
  (env-loop var env null-action eq-action))

(define (define-variable! var val env) ; assume env is not empty
  (define (null-action env)
    (add-binding-to-frame! var val (first-frame env)))
  (define (eq-action vals)
    (set-car! vals val))
  (env-loop var env null-action eq-action))

(define (set-variable-value! var val env)
  (define (null-action env)
    (env-loop var (enclosing-environment env) null-action eq-action))
  (define (eq-action vals)
    (set-car! vals val))
  (env-loop var env null-action eq-action))

(define (env-loop var env null-action eq-action)
  (define (scan vars vals)
    (cond ((null? vars)
           (null-action env))
          ((eq? var (car vars))
           (eq-action vals))
          (else
           (scan (cdr vars) (cdr vals)))))
  (if (empty-environment? env)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

;; We represent a frame as two lists of equal length: one of names and one
;; of values. Instead we could have represented a frame as a single list
;; of name-value pairs.
(define (make-frame variables values)
  (cons variables values))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define (empty-environment? env) (eq? env the-empty-environment))

(define the-empty-environment '())
