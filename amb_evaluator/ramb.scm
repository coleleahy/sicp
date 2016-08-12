(define (analyze-ramb exp)
  (let ((choice-procs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-another choices)
        (if (null? choices)
            (fail)
            ((let* ((idx (random (length choices)))
                    (choice (list-ref choices idx)))
               (choice env
                       succeed
                       (lambda ()
                         (try-another (remove choice choices))))))))
      (try-another choice-procs))))

(define (ramb-choices exp) (cdr exp))

(define (ramb? exp) (tagged-list? exp 'ramb))
