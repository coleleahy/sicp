(define (analyze-amb exp)
  (let ((choice-procs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next choice-procs))))

(define (amb-choices exp) (cdr exp))

(define (amb? exp) (tagged-list? exp 'amb))
