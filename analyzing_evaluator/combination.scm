;; Implements syntax and semantics of object-language combinations, i.e. sexps that
;; are neither special forms nor derived expressions.

(load (list "assignment"
            "eval-sequence"
            "quoted"
            "tagged-list"))

(define (analyze-combination exp)
  (let ((proc (analyze (operator exp))) ; analyze once and for all
        (args (map analyze (operands exp)))) ; analyze once and for all
    (lambda (env)
      (execute-application (proc env)
                           (map (lambda (x) (x env)) args)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc) ; proc's body is directly applicable to an environment
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (procedure-environment proc) (cadddr proc))

(define (procedure-body proc) (caddr proc))

(define (procedure-parameters proc) (cadr proc))

(define (make-procedure params body env)
  (list 'procedure
        params
        body
        env))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure)) 

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

;; Alternatively definable, using a higher-order procedure, as (map (lambda (x)
;; (eval x env)) exps). But eschewing higher-order procedures in the construction
;; of our Scheme REPL shows that an interpreter for a language with higher-order
;; functions can be implemented in a metalanguage that lacks such functions.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

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
