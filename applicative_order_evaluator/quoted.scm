;; Implements syntax (and therefore semantics) of object-language quotations.

(load "tagged-list")

(define (make-quoted text) (list 'quote text))

(define (text-of-quotation exp) (cadr exp))

(define (quoted? exp) (tagged-list? exp 'quote))
