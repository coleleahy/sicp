;; The list primitive-operations-table defined in this file determines which
;; operations can be referenced in controller-instruction subexpressions of form
;; "(op <operation-name>) <input1> ... <inputN>." For a minimal register machine,
;; there will be very few such operations. Here we have opted to include very many.

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
   ;; Compilation
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
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
   (list 'make-assignment make-assignment)
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
   ;; Case analyses
   (list 'cond? cond?)
   (list 'cond-clauses cond-clauses)
   (list 'cond-one-clause? cond-one-clause?)
   (list 'cond-first-clause cond-first-clause)
   (list 'cond-rest-clauses cond-rest-clauses)
   (list 'cond-else-clause? cond-else-clause?)
   (list 'cond-predicate cond-predicate)
   (list 'cond-actions cond-actions)
   ;; Lambdas
   (list 'make-procedure make-procedure)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   ;; Let expressions
   (list 'let? let?)
   (list 'let->combination let->combination)
   ;; Begin expressions
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'last-exp? last-exp?)
   (list 'no-more-exps? no-more-exps?)
   ;; Applications/combinations
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   (list 'last-operand? last-operand?)
   (list 'symbol? symbol?)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)))
