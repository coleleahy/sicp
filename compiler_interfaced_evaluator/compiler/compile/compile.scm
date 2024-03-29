(define (compile exp target linkage compile-time-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage compile-time-env))
        ((quoted? exp)
         (compile-quoted exp target linkage compile-time-env))
        ((variable? exp)
         (compile-variable exp target linkage compile-time-env))
        ((plus? exp)
         (compile-plus exp target linkage compile-time-env))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-env))
        ((if? exp)
         (compile-if exp target linkage compile-time-env))
        ((lambda? exp)
         (compile-lambda exp target linkage compile-time-env))
        ((let? exp)
         (compile-let exp target linkage compile-time-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage compile-time-env))
        ((cond? exp)
         (compile (cond->if exp) target linkage compile-time-env))
        ((application? exp)
         (compile-application exp target linkage compile-time-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))
