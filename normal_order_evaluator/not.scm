;; Implements syntax and semantics of object-language negations.

(load (list "tagged-list"
            "true-false"))

(define (eval-not exp env)
  (if (true? (actual-value (not-negatum exp) env))
      false
      true))

(define (make-not negatum) (list 'not negatum))

(define (not-negatum exp) (cadr exp))

(define (not? exp) (tagged-list? exp 'not))
