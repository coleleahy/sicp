;; Implements syntax and reduction of object-language case-analysis expressions

(load (list "begin"
            "if"
            "tagged-list.scm"))

(define (cond->if exp)
  (cond-expand-clauses (cond-clauses exp)))

(define (cond-expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
         (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND-IF"
                          clauses)))
              ((cond-arrow-clause? first)
               (let ((test (cond-predicate first))
                     (recipient (cadr (cond-actions first))))
                 (make-if test
                          (recipient test)
                          (cond-expand-clauses rest))))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (cond-expand-clauses rest)))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-arrow-clause? clause)
  (eq? (car (cond-actions clause)) '=>))

(define (cond-actions clause) (cdr clause))

(define (cond-predicate clause) (car clause))

(define (cond-clauses exp) (cdr exp))

(define (cond? exp) (tagged-list? exp 'cond))
