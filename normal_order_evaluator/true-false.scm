;; Defines handy Boolean predicates in the metalanguage

(define (false? val) (eq? false val))

(define (true? val) (not (false? val)))
