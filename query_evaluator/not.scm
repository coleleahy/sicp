;; Installer of evaluation procedure for not special form

;; The evaluation procedure defined here poses a problem for the view of the qeval
;; procedure according to which qeval-ing query-pattern relative to frame-stream
;; amounts to checking whether the propositional theory defined by the database
;; contains any closed instances, consistent with the bindings of some element of
;; frame-stream, of the formula described by query-pattern. For on that view, applying
;; qeval to a query-pattern of the form (not <query-subpattern>) relative to the empty
;; frame-stream should amount to checking whether the propositional theory contains any
;; closed instances of (not <query-subpattern>), or in other words (since the theory is
;; complete) whether the theory *fails to contain* certain closed instances of the
;; negatum <query-subpattern>. The problem is that, if the theory contains at least
;; one closed instance of <query-subpattern>, then qeval-ing (not <query-subpattern>)
;; relative to the empty frame-stream will not return any matches -- even if the
;; theory does contain closed instances of (not <query-subpattern>). In short, the
;; evaluation procedure for negations is flawed because it can lead to false negatives
;; when querying the database.

(define (install-not) (put negate 'not 'qeval))

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (negated-query exps) (car exps))
