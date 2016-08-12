;; Implements syntax and semantics of object-language combinations, i.e. sexps that
;; are neither special forms nor derived expressions.

(load (list "assignment"
            "eval-sequence"
            "quoted"
            "tagged-list"))

;; By design, the eval procedure passes to apply a procedure (i.e. the actual value of
;; an operator) plus some operands and an environment. If the procedure is compound,
;; the operands are transformed into thunks (i.e. objects packaging the ingredients
;; needed to determine the actual values of the operands) and the procedure's body is
;; evaluated in an extended environment where the procedure's parameters are bound
;; to the thunks. An operand's actual value is needed only when the operand is the
;; predicate clause of a conditional that is being evaluated, or when the operand is
;; the operator in a combination that is being evaluated, or when applying a primitive
;; procedure to the operand. In those cases, the corresponding thunk is used to compute
;; the operand's actual value.
(define (apply proc operands env)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure
          proc
          (list-of-values operands env))) ; need actual values of operands
        ((compound-procedure? proc)
         (eval-sequence
          (procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           (list-of-thunks operands env) ; don't yet need actual values of operands
           (procedure-environment proc))))
        (else
           (error "Unknown procedure type -- APPLY" proc))))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (procedure-environment proc) (cadddr proc))

(define (procedure-body proc) (caddr proc))

(define (procedure-parameters proc) (cadr proc))

(define (make-procedure params body env)
  (list 'procedure
        params
        (scan-out-defines body)
        env))

;; If proc-body contains any definitions, convert proc-body to a let-expression
;; in which the defined variables are initially set to a dummy value, and only
;; later set to the desired values. The effect is that the definitions internal
;; to proc-body are "simultaneous" in the sense that the definitions have the
;; same scope, namely the whole of proc-body. This simultaneous-scoping rule
;; stands in contrast to the sequential-scoping rule according to which the
;; scope of an internal definition would be the portion of proc-body beginning
;; with the given definition, excluding earlier parts of proc-body.
(define (scan-out-defines proc-body)
  (define (iter old-body bindings body)
    (if (null? old-body)
        (list bindings body)
        (let ((first-exp (first-exp old-body))
              (rest-exps (rest-exps old-body)))
          (cond ((definition? first-exp)
                 (iter rest-exps
                       (append bindings
                               (list
                                (list (definition-variable first-exp)
                                      (make-quoted '*unassigned*))))
                       (append body
                               (list (make-assignment
                                      (definition-variable first-exp)
                                      (definition-value first-exp))))))
                (else
                 (iter rest-exps
                       bindings
                       (append body
                               (list first-exp))))))))
  (let* ((f (lambda (exp) (if (list? exp) (car exp) exp))) ; exp could be a mere name
         (first-words (map f proc-body))) ; Could eliminate higher-order procedure.
    (cond ((memq 'define first-words)
           (let* ((bindings-and-body (iter proc-body '() '()))
                  (bindings (car bindings-and-body))
                  (body (cadr bindings-and-body)))
             (list (make-let bindings body))))
          (else proc-body))))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure)) 

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (list-of-values operands env)
  (if (no-operands? operands)
      '()
      (cons (actual-value (first-operand operands) env)
            (list-of-values (rest-operands operands) env))))

(define (list-of-thunks operands env)
  (if (no-operands? operands)
      '()
      (cons (delay-it (first-operand operands) env)
            (list-of-thunks (rest-operands operands) env))))

(define (rest-operands ops) (cdr ops))

(define (first-operand ops) (car ops))

(define (no-operands? ops) (null? ops))

(define (operands exp) (cdr exp))

(define (operator exp) (car exp))

;; The following definition works because of the structure of the defintion of the
;; eval procedure in eval.scm, where the *last clause* in the case analysis checks
;; whether the expression is a combination. This definition would not work if the
;; clause for combinations came earlier in the case analysis.
(define (combination? exp) (pair? exp))
