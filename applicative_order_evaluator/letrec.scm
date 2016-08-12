;; Implements syntax and reduction of object-language letrec-expressions

(load (list "assignment"
            "let"
            "quoted"
            "tagged-list"))

;; The reduction implemented here converts (letrec ((a ... a ... ) ,,, a ,,, )) into
;; (let ((a '*unassigned*)) (set! a ... a ... ) ,,, a ,,, ). There is, however, a
;; reduction that does not involve the assignment operator. To begin, we ensure that
;; (letrec ((a ... a ... ) ,,, a ,,, )) has form (letrec ((a (lambda (x) ::: a :::)))
;; ,,, a ,,, ) by adding x as dummy variable if necessary. Then we convert this form
;; to (let ((a (lambda (b) (lambda (x) ::: (b b) ::: )))) ,,, (a a) ,,, ), which is
;; our desired implementation of letrec. Informally, we are binding a to a function
;; that accepts a parameter b which determines which function (b b) we will call when
;; the argument x does not satisfy one of the base cases. Then, in the context that
;; follows the binding, we are passing a to itself as this parameter, so that in we
;; obtain a function (a a) that *calls itself* when the argument x does not satisfy
;; one of the base cases. Thus our alternative implementation permits us to call a
;; recursive function in the body of the let. Compare how the Y combinator permits
;; us to call a recursive function without binding it to a global name.
(define (letrec->let exp)
  (define (iter old-bindings bindings body)
    (if (null? old-bindings)
        (list bindings body)
        (let ((first-binding (car old-bindings))
              (rest-bindings (cdr old-bindings)))
          (iter rest-bindings
                (append bindings
                        (list (list (car first-binding)
                                    (make-quoted '*unassigned*))))
                (append body
                        (list (make-assignment
                                     (car first-binding)
                                     (cadr first-binding))))))))
  (let* ((bindings-and-body
          (iter (letrec-bindings exp) '() '()))
         (bindings (car bindings-and-body))
         (body (cadr bindings-and-body)))
    (make-let bindings (append body
                               (letrec-body exp)))))

(define (letrec-body exp) (cddr exp))

(define (letrec-bindings exp) (cadr exp))

(define (letrec? exp) (tagged-list? exp 'letrec))
