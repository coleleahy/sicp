;; Installer of evaluation procedure for and special form

(define (install-and) (put conjoin 'and 'qeval))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(define (rest-conjuncts exps) (cdr exps))

(define (first-conjunct exps) (car exps))

(define (empty-conjunction? exps) (null? exps))
