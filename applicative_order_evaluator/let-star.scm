;; Implements syntax and reduction of object-language let*-expressions

(load (list "let"
            "tagged-list"))

(define (let*->let exp)
  (let*-expand-bindings (let*-bindings exp)
                        (let*-body exp)))

(define (let*-expand-bindings bindings body)
  (let ((first (car bindings))
        (rest (cdr bindings)))
    (if (null? rest)
        (make-let (list first) body)
        (make-let (list first)
                  (let*-expand-bindings rest body)))))

(define (let*-body exp) (caddr exp))

(define (let*-bindings exp) (cadr exp))

(define (let*? exp) (tagged-list? exp 'let*))
