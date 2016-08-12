;; Installer of evaluation procedure for lisp-value special form

;; The evaluation procedure defined here poses a problem similar to that raised by
;; the evaluation procedure for negated queries.

(define (install-lisp-value) (put lisp-value 'lisp-value 'qeval))

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pattern variable -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (args exps) (cdr exps))

(define (predicate exps) (car exps))
