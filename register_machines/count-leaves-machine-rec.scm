(define count-leaves-controller-rec
  '((assign val (const 0))
    (assign continue (label *done))
    *loop
    (test (op null?) (reg tree))
    (branch (label *null-tree))
    (test (op pair?) (reg tree))
    (branch (label *non-leaf))
    *leaf
    (assign val (op +) (reg val) (const 1))
    (goto (reg continue))
    *non-leaf
    (save continue)
    (save tree)
    (assign tree (op car) (reg tree))
    (assign continue (label *after-count-car))
    (goto (label *loop))
    *after-count-car
    (restore tree)
    (restore continue)
    (assign tree (op cdr) (reg tree))
    (assign continue (label *after-count-cdr))
    (goto (label *loop))
    *after-count-cdr
    (restore tree)
    (restore continue)
    (goto (reg continue))
    *null-tree
    (goto (reg continue))
    *done))

(define count-leaves-machine-rec
  (make-machine '() count-leaves-controller-rec))
