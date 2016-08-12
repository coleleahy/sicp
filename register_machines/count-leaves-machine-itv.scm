(define count-leaves-controller-itv
  '((assign n (const 0))
    (assign continue (label *done))
    *loop
    (test (op null?) (reg tree))
    (branch (label *null-tree))
    (test (op pair?) (reg tree))
    (branch (label *non-leaf))
    *leaf
    (assign n (op +) (reg n) (const 1))
    (goto (reg continue))
    *non-leaf
    (save tree)
    (save continue)
    (assign continue (label *after-count-car))
    (assign tree (op car) (reg tree))
    (goto (label *loop))
    *after-count-car
    (restore tree)
    (assign tree (op cdr) (reg tree))
    (assign continue (label *after-count-cdr))
    (goto (label *loop))
    *after-count-cdr
    (restore continue)
    (goto continue)
    *null-tree
    (goto (reg continue))
    *done))

(define count-leaves-machine-itv
  (make-machine '() count-leaves-controller-itv))
