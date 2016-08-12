;; Implements primitive procedures of the object language in terms of procedures
;; (possibly non-primitive) of the metalanguage.

;; Could eliminate use of higher-order procedures.
(define (primitive-procedure-names) (map car primitive-procedures))

;; Could eliminate use of higher-order procedures.
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define primitive-procedures
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '<= <=)
        (list '> >)
        (list '>= >=)
        (list 'append append)
        (list 'apply apply-in-underlying-scheme)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'get-universal-time get-universal-time)
        (list 'null? null?)
        ;; <more primitives>
        ))
