(define (compile-let exp target linkage)
  (compile-application (let->combination exp) target linkage))
