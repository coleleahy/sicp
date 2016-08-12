;; Implements syntax of object-language begin-expressions.

(load "tagged-list")

(define (make-begin seq) (cons 'begin seq))

(define (begin-actions exp) (cdr exp))

(define (begin? exp) (tagged-list? exp 'begin))
