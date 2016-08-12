;; Controls the evaluation of a Scheme expression in an environment by dispatching
;; on its syntactic type to select the appropriate evaluation procedure. The
;; evaluation procedures are "lazy" in that the actual value of a subexpression is
;; computed only when absolutely necessary -- viz. when the subexpression is the
;; predicate clause of a conditional that is being evaluated, or when the subexpression
;; is the operator in a combination that is being evaluated, or when the subexpression
;; is an operand in a combination that is being evaluated whose operator denotes a
;; primitive procedure. In other words, a "normal-order" evaluation strategy is
;; implemented here, in contrast to the "applicative-order" strategy standardly
;; required of Scheme evaluators. Furthermore, in main.scm we install cons, car, and
;; cdr as compound procedures, which are therefore non-strict in their arguments --
;; yielding an evaluator that can handle infinite lists without requiring streams or
;; any other special data type.

(load (list "assignment"
            "begin"
            "combination"            
            "cond"
            "definition"
            "if"
            "lambda"
            "not"
            "quoted"
            "self-evaluating"
            "variable"))

(define (eval exp env)
  (cond
   ;; Primitive expressions
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ;; Special forms
   ((quoted? exp) (eval-quoted exp env)) ; complicated b/c we made cons non-strict
   ((assignment? exp) (eval-assignment exp env))
   ((definition? exp) (eval-definition exp env))
   ((if? exp) (eval-if exp env))
   ((not? exp) (eval-not exp env))
   ((lambda? exp)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
   ((begin? exp)
    (eval-sequence (begin-actions exp) env))
   ;; Derived expressions   
   ((cond? exp) (eval (cond->if exp) env))
   ;; Combinations
   ((combination? exp)
    (apply (actual-value (operator exp) env) ; need actual value for apply's dispatch
           (operands exp)
           env))
   (else
    (error "Unknown expression type -- EVAL" exp))))
