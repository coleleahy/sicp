;; Implements syntax and reduction of object-language while-expressions

(load (list "begin"
            "tagged-list"))

(define (while->if exp) ; looping condition met
  (make-if (while-predicate exp)
           (make-begin (append (while-body)
                               exp))
           'false)) ; looping condition not met

(define (while-body exp) (caddr exp))

(define (while-predicate exp) (cadr exp))

(define (while? exp) (tagged-list? exp 'while))
