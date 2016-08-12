(define fact-controller '(*start
                          (assign n (op read))
                          (perform (op initialize-stacks))
                          (perform (op initialize-inst-counter))
                          (assign continue (label *fact-done))
                          *fact-loop
                          (test (op =) (reg n) (const 1))
                          (branch (label *base-case))
                          (save continue)
                          (save n)
                          (assign n (op -) (reg n) (const 1))
                          (assign continue (label *after-fact))
                          (goto (label *fact-loop))
                          *after-fact
                          (restore n)
                          (restore continue)
                          (assign val (op *) (reg n) (reg val))
                          (goto (reg continue))
                          *base-case
                          (assign val (const 1))
                          (goto (reg continue))
                          *fact-done
                          (perform (op print-inst-counter))
                          (perform (op print) (reg val))
                          (perform (op print-stack-statistics))
                          (perform (op print) (const ""))
                          (goto (label *start))))

(define fact-machine
  (make-machine '() fact-controller))
