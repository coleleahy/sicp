;; Controls the evaluation of a Scheme expression in an environment.

;; Greater modularity could be achieved by implementing analyze in a data-directed
;; style as a hash table, since adding a new expression type would then not require
;; edits to the definition of analyze, but would rather only require the author of
;; the package for the new expression type to install that package in the hash table,
;; which can be done without editing the present file.

(load (list "amb"
            "assignment"
            "begin"
            "combination"            
            "cond"
            "definition"
            "if"
            "lambda"
            "let"
            "not"
            "quoted"
            "self-evaluating"
            "variable"))

;; Applies the "sense" of our expression to the environment in which we want the
;; expression to be evaluated, together with a success continuation and a failure
;; continuation. A success continuation is a procedure of two arguments: the value
;; just obtained and another failure continuation to be used if that value leads to
;; a subsequent failure. A failure continuation is a procedure of no arguments. 
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;; Returns a function that takes an environment, together with a success continuation
;; and a failure continuation, and returns either the result of applying the success
;; continuation to the denotation of the expression (plus the failure continuation) or
;; the result of evaluating the failure continuation.
(define (analyze exp)
  (cond
   ;; Primitive expressions
   ((self-evaluating? exp) (analyze-self-evaluating exp))
   ((variable? exp) (analyze-variable exp))
   ;; Special forms
   ((amb? exp) (analyze-amb exp))
   ((quoted? exp) (analyze-quoted exp))
   ((assignment? exp) (analyze-assignment exp))
   ((definition? exp) (analyze-definition exp))
   ((if? exp) (analyze-if exp))
   ((not? exp) (analyze-not exp))
   ((lambda? exp) (analyze-lambda exp))
   ((begin? exp) (analyze-sequence (begin-actions exp)))
   ;; Derived expressions
   ((cond? exp) (analyze (cond->if exp)))
   ((let? exp) (analyze (let->combination exp)))
   ;; Combinations
   ((combination? exp) (analyze-combination exp))
   (else
    (error "Unknown expression type -- ANALYZE" exp))))
