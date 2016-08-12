;; Controls the evaluation of a Scheme expression in an environment by dispatching
;; on its syntactic type to select the appropriate evaluation procedure.

;; Greater modularity could be achieved by implementing eval in a data-directed
;; style as a hash table, since adding a new expression type would then not require
;; edits to the definition of eval, but would rather only require the author of
;; the package for the new expression type to install that package in the hash table,
;; which can be done without editing the present file.

(load (list "and"
            "assignment"
            "begin"
            "combination"            
            "cond"
            "definition"
            "if"
            "lambda"
            "let"
            "let-star"
            "letrec"
            "not"
            "or"
            "quoted"
            "self-evaluating"
            "unbind"
            "variable"
            "while"))

(define (eval exp env)
  (cond
   ;; Primitive expressions
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ;; Special forms
   ((quoted? exp) (text-of-quotation exp))
   ((assignment? exp) (eval-assignment exp env))
   ((definition? exp) (eval-definition exp env))
   ((if? exp) (eval-if exp env))
   ((not? exp) (eval-not exp env))
   ((unbind? exp) (eval-unbind exp env))
   ((lambda? exp)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
   ((begin? exp)
    (eval-sequence (begin-actions exp) env))
   ;; Derived expressions   
   ((cond? exp) (eval (cond->if exp) env))
   ((and? exp) (eval (and->not exp) env))
   ((or? exp) (eval (or->if exp) env))
   ((let? exp) (eval (let->combination exp) env))
   ((let*? exp) (eval (let*->let exp) env))
   ((letrec? exp) (eval (letrec->let exp) env))
   ((while? exp) (eval (while->if exp) env))
   ;; Combinations
   ((combination? exp)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))
   (else
    (error "Unknown expression type -- EVAL" exp))))
