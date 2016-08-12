;; See SICP Exercise 5.25.

(define ece-controller
  '(*read-eval-print-loop
    (perform (op initialize-stacks))
    (perform (op prompt-for-input) (const ";;; ECL-Eval input:"))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label *print-result))
    (goto (label *actual-value))
    *print-result
    (perform (op announce-output) (const ";;; ECL-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label *read-eval-print-loop))

    *eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label *ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label *ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label *ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label *ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label *ev-definition))
    (test (op if?) (reg exp))
    (branch (label *ev-if))
    (test (op lambda?) (reg exp))
    (branch (label *ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label *ev-begin))
    (test (op application?) (reg exp))
    (branch (label *ev-application))
    (goto (label *unknown-expression-type))

    *actual-value
    (save continue)
    (assign continue (label *force-it))
    (goto (label *eval-dispatch))
    *force-it
    (test (op thunk?) (reg val))
    (branch (label *thunk))
    (restore continue)
    (goto (reg continue))
    *thunk
    (assign exp (op thunk-exp) (reg val))
    (assign env (op thunk-env) (reg val))
    (goto (label *actual-value))

    *ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
    
    *ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))
    
    *ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))

    *ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label *ev-assignment-1))
    (goto (label *eval-dispatch))
    *ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    *ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label *ev-definition-1))
    (goto (label *eval-dispatch))
    *ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    *ev-if
    (save exp)
    (assign exp (op if-predicate) (reg exp))
    (save continue)
    (assign continue (label *ev-if-did-predicate))
    (goto (label *actual-value))
    *ev-if-did-predicate
    (restore continue)
    (test (op true?) (reg val))
    (branch (label *ev-if-consequent))
    *ev-if-alternative
    (restore exp)
    (assign exp (op if-alternative) (reg exp))
    (goto (label *eval-dispatch))
    *ev-if-consequent
    (restore exp)
    (assign exp (op if-consequent) (reg exp))
    (goto (label *eval-dispatch))
    
    *ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    *ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label *ev-sequence))

    *ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label *ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label *ev-sequence-continue))
    (goto (label *eval-dispatch))
    *ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label *ev-sequence))
    *ev-sequence-last-exp
    (restore continue)
    (goto (label *eval-dispatch))    

    *ev-application
    (save continue)
    (assign unev (op operands) (reg exp))
    (assign exp (op operator) (reg exp))
    (assign continue (label *ev-appl-did-operator))
    (goto (label *actual-value))
    *ev-appl-did-operator
    (restore continue)
    (assign proc (reg val))
    (assign argl (op empty-arglist))
    (test (op no-operands?) (reg unev))
    (branch (label *apply-dispatch))
    (test (op primitive-procedure?) (reg val))
    (branch (label *ev-appl-primitive-loop))
    (test (op compound-procedure?) (reg val))
    (branch (label *ev-appl-compound-loop))
    (goto (label *unknown-procedure-type))    
    *ev-appl-compound-loop
    (assign exp (op first-operand) (reg unev))
    (assign val (op delay-it) (reg exp) (reg env))
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (test (op last-operand?) (reg unev))
    (branch (label *apply-dispatch))
    (assign unev (op rest-operands) (reg unev))
    (goto (label *ev-appl-compound-loop))    
    *ev-appl-primitive-loop
    (assign exp (op first-operand) (reg unev))
    (save continue)
    (save unev)
    (assign continue (label *ev-appl-primitive-did-operand))
    (goto (label *actual-value))
    *ev-appl-primitive-did-operand
    (restore unev)
    (restore continue)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (test (op last-operand?) (reg unev))
    (branch (label *apply-dispatch))
    (assign unev (op rest-operands) (reg unev))
    (goto (label *ev-appl-primitive-loop))

    *unknown-expression-type
    (assign val (const *unknown-expression-type-error))
    (goto (label *signal-error))
    *unknown-procedure-type
    (restore continue)
    (assign val (const *unknown-procedure-type-error))
    (goto (label *signal-error))
    *signal-error
    (perform (op user-print) (reg val))
    (goto (label *read-eval-print-loop))    

    *apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label *primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label *compound-apply))
    (goto (label *unknown-procedure-type))

    *primitive-apply
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (restore continue)
    (goto (reg continue))

    *compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label *ev-sequence))))
