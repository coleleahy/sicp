;; Implements syntax and semantics of object-language combinations, i.e. sexps that
;; are neither special forms nor derived expressions.

(load (list "assignment"
            "eval-sequence"
            "quoted"
            "tagged-list"))

(define (analyze-application exp)
  (let ((fxn-proc (analyze (operator exp)))
        (arg-procs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fxn-proc env
                (lambda (fxn fail2)
                  (get-args arg-procs
                            env
                            (lambda (args fail3)
                              (execute-application
                               fxn args succeed fail3))
                            fail2))
                fail))))

(define (get-args arg-procs env succeed fail)
  (if (null? arg-procs)
      (succeed '() fail)
      ((car arg-procs) env
                       (lambda (arg fail2) ; success continuation for this arg-proc
                         (get-args (cdr arg-procs)
                                   env
                                   (lambda (args fail3) ; success for call to get-args
                                     (succeed (cons arg args)
                                              fail3))
                                   fail2))
                       fail))) ; failure continuation for this arg-proc

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

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
