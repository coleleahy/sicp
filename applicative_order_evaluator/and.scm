;; Implements syntax and reduction of object-language conjunctions

(load (list "if" 
            "not"
            "tagged-list"))

(define (and->not exp)
  (let ((first (car (and-clauses exp)))
        (second (cadr (and-clauses exp))))
    (make-not (make-if first
                       (make-not second)))))

(define (and-clauses exp) (cdr exp))

(define (and? exp) (tagged-list? exp 'and))
