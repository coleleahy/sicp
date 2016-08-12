(pp (compile '(define (factorial n)
                (define (iter product counter)
                  (if (> counter n)
                      product
                      (iter (* counter product)

                            (+ counter 1))))
                (iter 1 1))
             'val
             'next))

;; COMPILED ITERATIVE DEFINITION
;; compile the definition-value (a lambda after scan-out-defines) with val/next
 ((assign val (op make-compiled-procedure) (label entry29) (reg env))
  (goto (label after-lambda28))
;; compile the lambda-body (a let expression, so use let->combination) targeting entry29
  entry29 ; calls to factorial enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; compile the single application constituting lambda-body with val/return
;; compile the operator expression (a lambda exp with parameter iter) with proc/next
  (assign proc (op make-compiled-procedure) (label entry31) (reg env))
  (goto (label after-lambda30))
  entry31 ; the lambda expression defining the operator exp enters here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (iter)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry36) (reg env))
  (goto (label after-lambda35))
  entry36 ; iter will enter here after it is reassigned from *unassigned*
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env)) ; begin (> counter n)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch51))
  compiled-branch50
  (assign continue (label after-call49))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch51
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call49 ; val now contains (> counter n)
  (restore env)
  (restore continue)
  (test (op false?) (reg val)) ; begin either product or (iter (* counter product) ...)
  (branch (label false-branch38))
  true-branch39 ; then clause of iter's if-then-else: product
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
  false-branch38 ; else clause of iter's if-then-else: (iter (* counter product) ...)
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue) ; ON EACH ITER CALL continue gets saved here
  (save proc) ; save iter before computing (* counter product) and (+ counter 1)
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch45))
  compiled-branch44
  (assign continue (label after-call43))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch45
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call43
  (assign argl (op list) (reg val)) ; accumulate (+ counter 1) into arg list
  (restore env)
  (save argl) ; save partial arg list ((+ counter 1)) for *
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch42))
  compiled-branch41
  (assign continue (label after-call40))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch42
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call40
  (restore argl) ; restore arg list, viz. ((+ counter 1))
  (assign argl (op cons) (reg val) (reg argl)) ; accumulate (* counter product) to argl
  (restore proc) ; restore iter
  (restore continue) ; continue gets restored BEFORE NESTING ANOTHER CALL TO ITER
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch48))
  compiled-branch47
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ; goto iter's entry point -- STACK HAS SAME DEPTH as at prev entry
  primitive-branch48
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue)) ; calls to iter exit here
  after-call46 ; there's nothing left to do -- contrast with recursive's after-call26
  after-if37
  after-lambda35 ; val now contains the desired entry to iter
  (perform (op set-variable-value!) (const iter) (reg val) (reg env)) ; reassign iter
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch34))
  compiled-branch33
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch34
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue)) ; goto the new entry of iter, viz. entry36
  after-call32 ; val now contains (iter 1 1), i.e. (factorial n)
  after-lambda30 ; proc contains value of (lambda (iter) (set! iter ... (iter 1 1)))
  (assign val (const *unassigned*))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch54))
  compiled-branch53
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ; goto the entry of compiled (lambda (iter) ... ), viz. entry31
  primitive-branch54
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call52 ; val now contains (factorial n)
  after-lambda28 ; proc now contains entry to compiled lambda defining factorial
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok)))
