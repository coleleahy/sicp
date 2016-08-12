(define (compile-definition exp target linkage compile-time-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next compile-time-env)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence
                  '(env val) (list target)
                  `((perform (op define-variable!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))
