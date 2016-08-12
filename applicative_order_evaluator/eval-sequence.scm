;; Implements syntax and semantics of sequences of expressions.

;; Required by: eval.scm

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (rest-exps seq) (cdr seq))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))
