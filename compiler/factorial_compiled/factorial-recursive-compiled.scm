(pp (compile '(define (factorial n)
                (if (= n 1)
                    1
                    (* (factorial (- n 1)) n)))
             'val
             'next))

;; COMPILED RECURSIVE DEFIINITION
;; compile the definition-value (a lambda) with val/next
 ((assign val (op make-compiled-procedure) (label entry19) (reg env))
  (goto (label after-lambda18))
;; compile the lambda-body (an if expression) targeting entry19
  entry19 ;; calls to factorial enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; compile the single if-expression constituting lambda-body with val/return
;; compile the predicate expression, viz. (= n 1)
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch34))
  compiled-branch33
  (assign continue (label after-call32))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch34
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call32 ; val now contains result of evaluating (= n 1)
;; compile consequent or alternative, viz. 1 or (* (factorial (- n 1)) n)
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch21))
  true-branch22 ; compile consequent
  (assign val (const 1))
  (goto (reg continue))
  false-branch21 ; compile alternative
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue) ; ON EACH FACTORIAL CALL continue gets saved here
  (save proc) ; ON EACH FACTORIAL CALL save * before computing (factorial (- n 1)) and n
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl) ; ON EACH FACTORIAL CALL save partial arg list (n) for *
;; evaluate (factorial (- n 1)), which is the other arg for *
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc) ; save factorial
;; evaluate (- n 1), which is the argument for factorial
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))
  compiled-branch24
  (assign continue (label after-call23))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch25
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call23 ; val now contains value of (- n 1)
  (assign argl (op list) (reg val))
  (restore proc) ; restore factorial
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch28))
  compiled-branch27
  (assign continue (label after-call26))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ; goto factorial's entry point, PILING UP THE STACK
  primitive-branch28
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call26 ; val now contains value of (factorial (- n 1))
  (restore argl) ; FINALLY restore partial arg list for *
  (assign argl (op cons) (reg val) (reg argl)) ; arg list is now complete
  (restore proc) ; FINALLY restore *
  (restore continue) ; FINALLY continue gets restored
;; apply * and return its value
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch31))
  compiled-branch30
;; compound procedure would be called tail-recursively ...
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch31
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call29
  after-if20
  after-lambda18
;; assign the procedure to the variable (factorial) naming it
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok)))
