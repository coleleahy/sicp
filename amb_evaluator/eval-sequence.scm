;; Implements syntax and semantics of sequences of expressions.

;; Required by: eval.scm

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2) ; continuation if value found for a
           (b env succeed fail2))
         fail))) ; continuation if dead end reached for a
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence of expressions -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (rest-exps seq) (cdr seq))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))
