(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage (make-instruction-sequence
            '() (list target)
            `((assign ,target (const ,exp))))))
