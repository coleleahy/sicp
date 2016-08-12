;; Implements syntax and semantics of object-language negations.

(load (list "tagged-list"
            "true-false"))

(define (analyze-not exp)
  (let ((negatum (analyze (not-negatum exp)))) ; analyze once and for all
    (lambda (env) (if (true? (negatum env))
                      false
                      true))))

(define (make-not negatum) (list 'not negatum))

(define (not-negatum exp) (cadr exp))

(define (not? exp) (tagged-list? exp 'not))
