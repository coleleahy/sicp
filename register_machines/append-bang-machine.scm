(define append-bang-controller
  '((assign continue (label *done))
    *lp-loop
    (assign the-cdr (op cdr) (reg x))
    (test (op null?) (reg the-cdr))
    (branch (label *lp-done))
    *nontriv-cdr
    (save x)
    (assign x (reg the-cdr))
    (save continue)
    (assign continue (label *after-append-cdr))
    (goto (label *lp-loop))
    *after-append-cdr
    (restore continue)
    (assign the-cdr (reg x))
    (restore x)
    (perform (op set-cdr!) (reg x) (reg the-cdr))
    (goto (label *continue))
    *lp-done
    (perform (op set-cdr!) (reg x) (reg y))
    (goto (reg continue))
    *done))

(define append-bang-machine
  (make-machine '() append-bang-controller))
