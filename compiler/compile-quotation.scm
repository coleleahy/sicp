(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage (make-instruction-sequence
            '() (list target)
            `((assign ,target (const ,(text-of-quotation exp)))))))
