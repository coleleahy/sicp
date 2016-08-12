;; Compiling a source-language expression with respect to a target register and a
;; linkage produces an instruction sequence such that, after initializing the
;; sequence's required registers and executing the sequence's statements, the target
;; register will contain a representation of the expression's value (relative to
;; the values represented by the contents of the required registers).

(load
 (list
  "compile-application"
  "compile-assignment"
  "compile-cond"
  "compile-definition"
  "compile-if"
  "compile-lambda"
  "compile-let"
  "compile-plus" ; shows how to "open-code" source language's primitive procedures
  "compile-quotation"
  "compile-self-eval"
  "compile-sequence"
  "compile-variable"
  "instruction-seqs"
  "labels"
  "linkages"
  "primitive-ops-library"
  "primitive-ops-table"
  "register-machine"))
            

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp)
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((plus? exp)
         (compile-plus exp target linkage compile-time-env))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((let? exp)
         (compile-let exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp)
         (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))
