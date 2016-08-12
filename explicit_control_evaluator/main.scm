(load (list "ece-controller"
            "ece-primitive-operations"
            "generic-machine"
            "registers"
            "stacks"
            "required-registers"
            "assembler"
            "exec-procs"
            "exec-proc-assign"
            "exec-proc-test"
            "exec-proc-branch"
            "exec-proc-goto"            
            "exec-proc-restore"
            "exec-proc-save"
            "exec-proc-perform"))

(define ece-machine
  (let ((machine (make-new-machine)))
    ((machine 'install-controller) ece-controller)
    machine))

;; (ece-machine 'toggle-inst-tracing)

;; (ece-machine 'toggle-label-tracing)

;; ((get-register ece-machine 'unev) 'toggle-tracing)

;; ((get-register ece-machine 'exp) 'toggle-tracing)

(start ece-machine)
