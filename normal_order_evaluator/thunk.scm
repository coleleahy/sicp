;; Implements the processing of thunks as a tool for delaying the evaluation of
;; operands in object-language combinations.

(load "tagged-list")

;; Executes the delayed evaluation encoded in the thunk associated with exp and env.
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-expression obj)
                                     (thunk-environment obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (thunk-value obj) (cadr obj))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))

;; 'Make-thunk' would be a more consistent name.
(define (delay-it exp env) (list 'thunk exp env))

(define (thunk-environment obj) (caddr obj))

(define (thunk-expression obj) (cadr obj))

(define (thunk? obj) (tagged-list? obj 'thunk))
