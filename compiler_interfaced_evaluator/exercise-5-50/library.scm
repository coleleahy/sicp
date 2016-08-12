;; Compilation -------------------------------------------------------------------
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))

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
      (display object))
  (newline))

;; Performance monitoring --------------------------------------------------------
(define (initialize-instruction-counter)
  (set! inst-counter 0))

(define (initialize-stacks)
  (for-each
   (lambda (stack)
     ((cadr stack) 'initialize))
   (universal-machine 'stack-table)))

(define (print-instruction-counter)
  (display (list 'inst-counter '= inst-counter))
  (newline))

(define (print-stack-statistics)
  (for-each
   (lambda (stack)
     (((cadr stack) 'print-statistics)
      (car stack)))
   (universal-machine 'stack-table)))

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
      (error "Unbound variable -- ENV-LOOP" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (lexical-address-lookup lex-adr run-time-env)
  (let ((frame-num (car lex-adr))
        (displacement-num (cadr lex-adr)))
    (let ((the-frame (list-ref run-time-env frame-num)))
      (let ((the-vals (frame-values the-frame)))
        (let ((val (list-ref the-vals displacement-num)))
          (if (eq? '*unassigned* val)
              (error "Unassigned variable -- LEX-LOOKUP" lex-adr)
              val))))))

(define (lexical-address-set! lex-adr val run-time-env)
  (let ((frame-num (car lex-adr))
        (displacement-num (cadr lex-adr)))
    (let ((the-frame (list-ref run-time-env frame-num)))
      (let ((the-vals (frame-values the-frame)))
        (list-set! the-vals displacement-num val)))))

(define (compile-in-repl scheme-program) ; See SICP Exercise 5.48.
  (set-register-contents!
   scheme-repl-vm 'val
   (assemble-execution-procedures
    (translate-to-machine-lang scheme-program)))
  (set-register-contents!
   scheme-repl-vm 'flag
   'external-entry)
  (start scheme-repl-vm))

(define scheme-primitive-procedures
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
   (list 'apply-in-underlying-scheme 'apply-in-underlying-scheme)
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'list list)
   (list 'null? null?)
   (list 'map map)
   (list 'display display)
   (list 'compile-in-repl compile-in-repl))) ; See SICP Exercise 5.48.

(define (scheme-primitive-procedure-names) (map car scheme-primitive-procedures))

(define (scheme-primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       scheme-primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (scheme-primitive-procedure-names)
                             (scheme-primitive-procedure-objects)
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
(define (make-quoted text) (list 'quote text))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Assignments -------------------------------------------------------------------
(define (make-assignment var val) (list 'set! var val))

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
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (false? val) (eq? false val))

(define (true? val) (not (eq? false val)))

;; Cond expressions ---------------------------------------------------------------
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-one-clause? clauses) (null? (cdr clauses)))

(define (cond-first-clause clauses) (car clauses))

(define (cond-rest-clauses clauses) (cdr clauses))

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

;; Lambdas ------------------------------------------------------------------------
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-procedure params body env)
  (list 'procedure
        params
        body
        env))

;; Let expressions ----------------------------------------------------------------

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-parameters exp)
  (map (lambda (x) (car x)) (let-bindings exp)))

(define (let-expressions exp)
  (map (lambda (x) (cadr x)) (let-bindings exp)))

(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cond ((list? (let-bindings exp))
         (cons (make-lambda (let-parameters exp)
                            (let-body exp))
               (let-expressions exp)))
        (else
         (let* ((proc-name (named-let-procname exp))
                (proc-val (make-lambda (cons proc-name
                                             (named-let-parameters exp))
                                       (named-let-body exp)))
                (new-bindings (cons (list proc-name proc-val)
                                    (named-let-bindings exp))))
           (let->combination (make-let new-bindings
                                       (named-let-body exp)))))))

;; Begin expressions --------------------------------------------------------------
(define (make-begin seq) (cons 'begin seq))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (first-exp seq) (car seq))

(define (last-exp? seq) (null? (cdr seq)))

(define (rest-exps seq) (cdr seq))

(define (no-more-exps? seq) (null? seq))

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
