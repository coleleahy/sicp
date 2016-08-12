;; Implements syntax and reduction of object-language let-expressions.

(load (list "lambda"
            "tagged-list"))

(define (let->combination exp)
  (cond ((list? (let-bindings exp))
         (cons (make-lambda (let-parameters exp)
                            (let-body exp))
               (let-expressions exp)))
        (else
         (let* ((proc-name (named-let-procname exp))
                (proc-val (make-lambda (cons proc-name
                                             (named-let-parameters exp))
                                       (named-let-body exp)))
                (new-bindings (cons (list proc-name proc-val)
                                    (named-let-bindings exp))))
           (let->combination (make-let new-bindings
                                       (named-let-body exp)))))))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (named-let-body exp) (cdddr exp))

(define (named-let-parameters exp)
  (map (lambda (x) (car x)) (named-let-bindings exp)))

(define (named-let-bindings exp) (caddr exp))

(define (named-let-procname exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-expressions exp)
  (map (lambda (x) (cadr x)) (let-bindings exp)))

(define (let-parameters exp)
  (map (lambda (x) (car x)) (let-bindings exp)))

(define (let-bindings exp) (cadr exp))

(define (let? exp) (tagged-list? exp 'let))

