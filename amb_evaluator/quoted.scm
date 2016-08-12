;; Implements syntax and semantics of object-language quotations.

(load "tagged-list")

(define (analyze-quoted exp)
  (let ((val (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed val fail))))

(define (make-quoted text) (list 'quote text))

(define (text-of-quotation exp) (cadr exp))

(define (quoted? exp) (tagged-list? exp 'quote))
