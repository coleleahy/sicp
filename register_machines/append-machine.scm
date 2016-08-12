(define append-controller
  '((assign val (reg y))
    (assign continue (label *done))
    *loop
    (test (op? null?) (reg x))
    (branch (label *null-x))
    (assign the-car (op car) (reg x))
    (assign x (op cdr) (reg x))
    (save continue)
    (assign continue (label *after-append-cdr))
    (save the-car)
    (goto (label *loop))
    *after-append-cdr
    (restore the-car)
    (restore continue)
    (assign val (op cons) (reg the-car) (reg val))
    (goto (reg continue))
    *null-x
    (goto (reg continue))
    *done))

(define append-machine
  (make-machine '() append-controller))
