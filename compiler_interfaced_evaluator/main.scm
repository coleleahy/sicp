;; If Scheme REPL virtual machine is started in the body of the compile-before-repl
;; procedure, the flag register will contain the symbol external-entry and the val
;; register will contain machine instructions (with attached execution procedures)
;; for the compiled Scheme program. The statement (goto (reg val)) will be executed,
;; resetting the contents of the pc register to the given of machine instructions.
;; For example, if the Scheme program is a procedure definition then the pc register's
;; contents will be reset to a sequence of instructions that builds the procedure
;; object and binds the procedure name to that object. By design, the sequence of
;; instructions ends with the statement (goto (reg continue)) so that after binding
;; the procedure name to the appropriate object, the pc register will again be reset,
;; this time to the sequence of instructions following the *print-result label. After
;; this, the scheme-repl-virtual-machine continues to operate as usual, except that
;; it now has access to the compiled procedure object created at the outset. Thus
;; the compile-before-repl procedure does just what we wanted: provides optimized
;; execution, within a REPL environment, of functions we would like to treat as "black
;; boxes" insofar as we do not require the ability to inspect their Scheme definitons
;; but rather only require the ability to apply the functions and see the results.

(load
 (list
  "library"
  "scheme-repl-program"
  "universal-machine/registers"
  "universal-machine/stacks"
  "universal-machine/list-program-registers"
  "universal-machine/assembler"
  "universal-machine/universal-machine"
  "universal-machine/exec-procs/exec-procs"
  "universal-machine/exec-procs/assign"
  "universal-machine/exec-procs/branch"
  "universal-machine/exec-procs/goto"
  "universal-machine/exec-procs/perform"
  "universal-machine/exec-procs/restore"
  "universal-machine/exec-procs/save"
  "universal-machine/exec-procs/test"
  "compiler/instruction-seqs"
  "compiler/labels"
  "compiler/linkages"
  "compiler/compile/compile"
  "compiler/compile/application-improved"
  "compiler/compile/assignment"
  "compiler/compile/cond"
  "compiler/compile/definition"
  "compiler/compile/if"
  "compiler/compile/lambda"
  "compiler/compile/let"
  "compiler/compile/plus"
  "compiler/compile/quotation"
  "compiler/compile/self-eval"
  "compiler/compile/sequence"
  "compiler/compile/variable"))

(define universal-machine (make-universal-machine))

(define (translate-to-machine-lang scheme-program)
  (statements
   (compile scheme-program
            'val 'return
            the-empty-environment)))

(define (assemble-execution-procedures machine-program)
  (instructions-with-exec-procs
   (assemble machine-program universal-machine)))

(define scheme-repl-vm
  (install-program universal-machine scheme-repl-program))

(define (compile-before-repl scheme-program)
  (set-register-contents!
   scheme-repl-vm 'val
   (assemble-execution-procedures
    (translate-to-machine-lang scheme-program)))
  (set-register-contents! scheme-repl-vm 'flag 'external-entry)
  (start scheme-repl-vm))
