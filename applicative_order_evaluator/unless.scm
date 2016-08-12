;; Implements syntax and semantics of object-language unless-statements. The
;; evaluation procedure is "lazy" in that it evaluates neither the usual-value clause
;; nor the exceptional-value clause of the unless-statement until it is needed.
;; This allows us to evaluate expressions like (unless (= b 0) (/ a b) b) without
;; worrying that an error will arise in cases where 'b' is bound to 0.

;; Required by: eval.scm

(load "tagged-list")

(define (eval-unless exp env)
  (let
      ((t (cons-stream '() (eval (unless-exceptional-value exp) env)))
       (f (cons-stream '() (eval (unless-usual-value exp))))
       (result '*unassigned*))
    (if (true? (eval (unless-condition exp) env))
        (set! result t)
        (set! result f))
    (stream-cdr result)))
    
(define (unless-exceptional-value exp) (cadddr exp))

(define (unless-usual-value exp) (caddr exp))

(define (unless-condition exp) (cadr exp))

(define (unless? exp) (tagged-list? exp 'unless))
