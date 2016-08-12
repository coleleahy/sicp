;; Controls the evaluation of a Scheme expression in an environment. The process has
;; two stages. First, the "sense" of the expression -- that is, a function that takes
;; an environment and returns the "denotation" of the expression in the environment --
;; is computed by the analyze procedure. Next, that sense is applied to the desired
;; environment. This achieves greater efficiency than the original metacircular
;; evaluator because operations because it reduces the number of computations devoted
;; to syntactic analysis. For instance, to evaluate a combination whose operator 'f
;; is bound to the value of a lambda-expression, it is not necessary to plod through
;; the syntactic analysis of the lambda's body. For evaluating the lambda expression
;; yielded a procedure object whose "body" component is the sense of the body of the
;; lambda expression. Therefore, to evaluate a combination whose operator is 'f, we
;; simply apply the sense of the lambda expression's body to the current environment
;; extended with a binding of the combination's operands to their values. Each time
;; we evaluate such a combination, we save time by applying the sense of the lambda's
;; body instead of working with the syntax of that body.

;; Greater modularity could be achieved by implementing analyze in a data-directed
;; style as a hash table, since adding a new expression type would then not require
;; edits to the definition of analyze, but would rather only require the author of
;; the package for the new expression type to install that package in the hash table,
;; which can be done without editing the present file.

(load (list "assignment"
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

;; Applies the "meaning" of our expression to the environment in which we want the
;; expression to be evaluated.
(define (eval exp env) ((analyze exp) env))

;; Returns a function that takes an environment and returns the denotation of the
;; expression in that environment. We can think of this function as the "meaning" of
;; the expression. (Compare Frege's distinction between sense and denotation.)
(define (analyze exp)
  (cond
   ;; Primitive expressions
   ((self-evaluating? exp) (analyze-self-evaluating exp))
   ((variable? exp) (analyze-variable exp))
   ;; Special forms
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
