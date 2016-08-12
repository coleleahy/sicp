(define (compile-sequence seq target linkage compile-time-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-time-env)
      (preserving
       '(env continue)
       (compile (first-exp seq) target 'next compile-time-env)
       (compile-sequence (rest-exps seq) target linkage compile-time-env))))
