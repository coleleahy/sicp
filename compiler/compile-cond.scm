(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond-expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
         (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND-IF"
                          clauses)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (cond-expand-clauses rest)))))))

(define (cond->if exp)
  (cond-expand-clauses (cond-clauses exp)))

(define (compile-cond exp target linkage)
  (compile-if (cond->if exp) target linkage))
