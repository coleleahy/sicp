;; The compile-application procedure defined in SICP Section 5.5.3 constructs a
;; procedure application's argument list from right to left. If we are willing to
;; sacrifice some efficiency, we can construct the list from left to right:

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl
                     (op append) (reg argl) (reg val))))))) ; append is costly
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr-operand-codes))))))

(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence
       '() '(argl)
       '((assign argl (const ()))))
      (let ((code-to-get-first-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence
               '(val) '(argl)
               '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-first-arg
            (preserving
             '(env)
             code-to-get-first-arg
             (code-to-get-rest-args (cdr operand-codes)))))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val
                    (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val
                      (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((assign val
                    (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

(define (compile-application exp target linkage)
  (let ((proc-code
         (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))
