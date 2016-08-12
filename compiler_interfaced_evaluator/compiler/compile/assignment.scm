(define (compile-assignment exp target linkage compile-time-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next compile-time-env)))
    (let ((lex-adr
           (find-variable var compile-time-env)))
      (if (eq? 'not-found lex-adr)
          (end-with-linkage
           linkage
           (preserving
            '(env)
            get-value-code
            (make-instruction-sequence
             '(val) `(,target env)
             `((assign env (op get-global-environment))
               (perform (op set-variable-value!)
                        (const ,var)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))
          (end-with-linkage
           linkage
           (preserving
            '(env)
            get-value-code
            (make-instruction-sequence
             '(env val) (list target)
             `((perform (op lexical-address-set!)
                        (const ,lex-adr)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))))))
