;; The primitive-operations-table defined at the end of this file determines which
;; operations can be reference by in controller-instruction subexpressions of form
;; "(op <operation-name>) <input1> ... <inputN>." For a minimal register machine,
;; there will be very few such operations. Here we have opted to include very many,
;; in order to reveal the structure of the controller for the explicit control
;; evaluator, which otherwise would become obscured by a proliferation of low-level
;; syntax.

;; I/O ---------------------------------------------------------------------------
(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '*environment-component*))
      (display object)))

;; Performance monitoring --------------------------------------------------------
(define (initialize-inst-counter)
  (set! inst-counter 0))

(define (initialize-stacks)
  (for-each
   (lambda (stack)
     ((cadr stack) 'initialize))
   (ece-machine 'stack-table)))

(define (print-inst-counter)
  (display (list 'inst-counter '= inst-counter))
  (newline))

(define (print-stack-statistics)
  (for-each
   (lambda (stack)
     (((cadr stack) 'print-statistics)
      (car stack)))
   (ece-machine 'stack-table)))

;; Environments ------------------------------------------------------------------

(define the-empty-environment '())

(define (empty-environment? env) (eq? env the-empty-environment))

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied -- EXTEND-ENV" vars vals)
          (error "Too few arguments supplied -- EXTEND-ENV" vars vals))))

(define (define-variable! var val env) ; assume env is not empty
  (define (null-action env)
    (add-binding-to-frame! var val (first-frame env)))
  (define (eq-action vals)
    (set-car! vals val))
  (env-loop var env null-action eq-action))

(define (lookup-variable-value var env)
  (define (null-action env)
    (env-loop var (enclosing-environment env) null-action eq-action))
  (define (eq-action vals)
    (if (eq? '*unassigned* (car vals))
        (error "Unassigned variable -- LOOKUP" var)
        (car vals)))
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

(define primitive-procedures
  (list
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '/ /)
   (list '= =)
   (list '< <)
   (list '<= <=)
   (list '> >)
   (list '>= >=)
   (list 'append append)
   (list 'apply apply)
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? null?)))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (first-frame env) (car env))

(define (enclosing-environment env) (cdr env))

(define the-global-environment (setup-environment))

(define (get-global-environment) the-global-environment)

;; Self-evaluating expressions ---------------------------------------------------
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; Variables ---------------------------------------------------------------------
(define (variable? exp) (symbol? exp))

;; Quotations --------------------------------------------------------------------
(define (quoted? exp) (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (text-of-quotation exp) (cadr exp))

;; Assignments -------------------------------------------------------------------
(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; Definitions -------------------------------------------------------------------
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (scan-out-defines (cddr exp)))))

(define (scan-out-defines proc-body)
  (define (lacks-definitions? body)
    (let* ((compound-exps
            (filter list? body))
           (definitions
            (filter (lambda (x) (eq? 'define (car x)))
                    compound-exps)))
      (null? definitions)))
  (define (iter old-body new-bindings new-body)
    (if (null? old-body)
        (list new-bindings new-body)
        (let ((first-exp (first-exp old-body))
              (rest-exps (rest-exps old-body)))
          (cond ((definition? first-exp)
                 (let* ((the-variable
                         (definition-variable first-exp))
                        (the-binding
                         (list the-variable (make-quoted '*unassigned*)))
                        (the-value
                         (definition-value first-exp))
                        (the-assignment
                         (make-assignment the-variable the-value)))
                   (iter rest-exps
                         (append new-bindings (list the-binding))
                         (append new-body (list the-assignment)))))
                (else (iter rest-exps
                            new-bindings
                            (append new-body (list first-exp))))))))
  (if (lacks-definitions? proc-body)
      proc-body
      (let* ((let-bindings-and-body
              (iter proc-body '() '()))
             (let-bindings
              (car let-bindings-and-body))
             (let-body
              (cadr let-bindings-and-body)))
        (list (make-let let-bindings let-body)))))

;; Conditionals -------------------------------------------------------------------
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (true? val) (not (eq? false val)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; Lambdas ------------------------------------------------------------------------
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-procedure params body env)
  (list 'procedure
        params
        body
        env))

;; Begin expressions --------------------------------------------------------------
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (last-exp? seq) (null? (cdr seq)))

;; Cond expressions ---------------------------------------------------------------
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond-expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
         (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND-IF"
                          clauses)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (cond-expand-clauses rest)))))))

(define (cond->if exp)
  (cond-expand-clauses (cond-clauses exp)))

;; Applications/combinations ------------------------------------------------------
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (procedure-parameters proc) (cadr proc))

(define (procedure-body proc) (caddr proc))

(define (procedure-environment proc) (cadddr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc)
         args))

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

;; Master list --------------------------------------------------------------------
(define primitive-operations-table
  (list
   ;; Essential operations -- all others can be eliminated to obtain a minimal machine
   (list '= =)
   (list '< <)
   (list '+ +)
   (list '* *)
   (list 'read read)
   (list 'display display)
   (list 'vector-ref vector-ref)
   (list 'vector-set! vector-set!)
   ;; Arithmetic   
   (list '- -)
   (list '/ /)
   (list '> >)
   ;; I/O
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   ;; Performance monitoring
   (list 'initialize-inst-counter initialize-inst-counter)
   (list 'initialize-stacks initialize-stacks)
   (list 'print-inst-counter print-inst-counter)
   (list 'print-stack-statistics print-stack-statistics)
   ;; Environments
   (list 'the-global-environment the-global-environment)
   (list 'get-global-environment get-global-environment)
   (list 'extend-environment extend-environment)
   (list 'define-variable! define-variable!)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   ;; Self-evaluating expressions
   (list 'self-evaluating? self-evaluating?)
   ;; Variables
   (list 'variable? variable?)
   ;; Quotations
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   ;; Assignments
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   ;; Definitions
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   ;; Conditionals
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'true? true?)
   (list 'make-if make-if)
   ;; Lambdas
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'make-procedure make-procedure)
   ;; Begin expressions
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'last-exp? last-exp?)
   ;; Cond expressions
   (list 'cond? cond?)
   (list 'cond->if cond->if)
   ;; Applications/combinations
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   (list 'last-operand? last-operand?)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)))
