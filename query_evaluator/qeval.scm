;; Controls evaluation of database queries

;; Regarding the database as a specification of the atomic theory of a model, we may
;; regard the qeval procedure as a way of ascertaining whether the propositional
;; theory contains any closed instances, consistent with the bindings of some element
;; of frame-stream, of the formula described by query-pattern. The qeval procedure
;; dispatches on the syntactic type of query-pattern to select the appropriate
;; procedure for searching the propositional theory -- fetching the procedure from
;; a table if query-pattern is a special form, and calling the basic qeval-simple
;; procedure, which searches the atomic theory, if query-pattern is a simple query.

(define (qeval query frame-stream)
  (let ((qeval-special (get (type query) 'qeval)))
    (if qeval-special
        (qeval-special (contents query) frame-stream)
        (qeval-simple query frame-stream))))

