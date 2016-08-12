(define (compile-self-evaluating exp target linkage compile-time-env)
  (end-with-linkage
   linkage (make-instruction-sequence
            '() (list target)
            `((assign ,target (const ,exp))))))
