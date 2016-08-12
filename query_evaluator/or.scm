;; Installer of evaluation procedure for the or special form

(define (install-or) (put disjoin 'or 'qeval))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(define (rest-disjuncts exps) (cdr exps))

(define (first-disjunct exps) (car exps))

(define (empty-disjunction? exps) (null? exps))
