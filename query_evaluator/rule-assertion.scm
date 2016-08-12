;; Constructors and selectors for rules and assertions

(define (assertion-to-add-body exp)
  (car (contents exp)))

(define (assertion-to-add? exp)
  (eq? (type exp) 'assert!))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (rule? statement)
  (tagged-list? statement 'rule))
