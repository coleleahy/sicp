;; Implements syntax and conversion of object-language disjunctions

(load (list "if"
            "not"
            "tagged-list"))

(define (or->if exp)
  (let ((first (car (or-clauses exp)))
        (second (cadr (or-clauses exp))))
    (make-if (make-not first)
             second)))

(define (or-clauses exp) (cdr exp))

(define (or? exp) (tagged-list? exp 'or))
