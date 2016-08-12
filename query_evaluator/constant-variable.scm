;; Syntactic procedures for constants and variables

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define rule-counter 0)

(define (var? exp) (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))
