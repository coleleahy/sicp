;; We may regard this as a sketch of a controller for a minimal register machine --
;; that is, a machine that responds to controllers that reference only the simplest
;; operations in subexpressions of form "(op <operation-name>) <input1> ... <inputN>."
;; For example, these operations could be vector-ref, vector-set!, read, display, +, *,
;; and a few other essential functions. To obtain a controller for such a machine,
;; we would iterate a syntactic transformation on the below controller until reaching
;; a fixed point, which would be the desired controller. Each iteration would replace
;; instructions containing subexpressions "(op <operation-name>) <input1> ... <inputN>"
;; by a series of instructions in which "<operation-name>" does not appear. (See pages
;; 435 through 438 of SICP for examples.) Ultimately, every subexpression of the form
;; "(op <operation-name>) <input1> ... <inputN>" would reference only the simplest
;; operations. The very possibility of performing the envisioned sequence of syntactic
;; transformations shows that a Scheme interpreter is in no way "magical," since it
;; could be implemented by a controller for a minimal register machine. The below
;; controller elucidates the structure that such a controller would (or could) have,
;; and thus sheds light on several difficult questions about Scheme interpreters --
;; such as how interpreters manage, in the course of evaluating a compound expression,
;; to return the values of subexpressions to the subroutine waiting to use those values
;; toward computing the value of the enclosing expression.

(define ece-controller
  '(*read-eval-print-loop
    (perform (op initialize-stacks))
    (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label *print-result))
    (goto (label *eval-dispatch))
    *print-result
    (perform (op print-stack-statistics))
    (perform (op announce-output) (const ";;; EC-Eval value:"))
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
    (test (op cond?) (reg exp))
    (branch (label *ev-cond))
    (test (op let?) (reg exp))
    (branch (label *ev-let))
    (test (op application?) (reg exp))
    (branch (label *ev-application))
    (goto (label *unknown-expression-type))

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
    (save env)
    (save continue)
    (assign continue (label *ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label *eval-dispatch))
    *ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label *ev-if-consequent))
    *ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label *eval-dispatch))
    *ev-if-consequent
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
    *ev-sequence-last-exp ; Implements tail recursion by eschewing unnecessary save
    (restore continue) ; operations that would inflate the maximum stack depth.
    (goto (label *eval-dispatch))

    *ev-cond
    (assign exp (op cond->if) (reg exp))
    (goto (label *eval-dispatch))

    *ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label *eval-dispatch))

    *ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label *ev-appl-did-operator))
    (goto (label *eval-dispatch))
    *ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label *apply-dispatch))
    (save proc)
    *ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label *ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label *ev-appl-accumulate-arg))
    (goto (label *eval-dispatch))
    *ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label *ev-appl-operand-loop))
    *ev-appl-last-arg
    (assign continue (label *ev-appl-accum-last-arg))
    (goto (label *eval-dispatch))
    *ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label *apply-dispatch))
    
    *unknown-expression-type
    (assign val (const *unknown-expression-type-error))
    (goto (label *signal-error))
    *unknown-procedure-type
    (restore continue) ; cleans up stack after apply-dispatch
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
