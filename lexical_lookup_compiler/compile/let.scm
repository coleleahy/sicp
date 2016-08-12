(define (compile-let exp target linkage compile-time-env)
  (compile-application (let->combination exp) target linkage compile-time-env))
