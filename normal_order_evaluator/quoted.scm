;; Implements syntax and semantics of object-language quotations.

(load "tagged-list")

(define (eval-quoted exp env)
  (define (list->lazy items)
    (if (null? items)
        '()
        (list 'cons (list 'quote (car items)) (list->lazy (cdr items)))))
  (if (list? (text-of-quotation exp))
      (eval (list->lazy (text-of-quotation exp)) env)
      (text-of-quotation exp)))

;; Scheme expressions of the form '(a b c) are syntactic sugar for (list 'a 'b 'c).
;; So, since we have designed our normal-order evaluator so that cons is a compound
;; procedure (rather than a primitive) and therefore is non-strict in its arguments,
;; the evaluator should interpret ('quote '(a b c)) as a "lazy list," i.e. as a
;; list built up with the non-strict cons operation. This complicates the evaluation
;; procedure for object-language quotations: while a quotation ('quote 'abc) should
;; evaluate to its text 'abc, a quotation ('quote '(a b c)) should evaluate to the lazy
;; list whose first item is 'a and whose second and third items are 'b and 'c. This
;; lazy list is distinct from the text '(a b c) of the quotation, which is rather a
;; list built up with the strict cons operation of the metalanguage. [This explains
;; why our applicative-order evaluators can simply interpret ('quote '(a b c)) by
;; returning its text. For those evaluators take cons as a primitive operation that
;; mirrors the cons operation of the metalanguage, whence a list built up with the
;; cons operation of the object language is nothing but a list built up with the cons
;; operation of the metalanguage.] To correctly evaluate a quotation ('quote '(a b c))
;; in the present context, we must convert the text '(a b c) into an object-language
;; expression that evaluates to the appropriate lazy list, and then evaluate that
;; expression. The desired expression is ('cons ('quote 'a) ('cons ('quote 'b) ('cons
;; ('quote 'c) '()))).
(define (eval-quoted exp env)
  (let ((text (text-of-quotation exp)))
    (cond ((pair? text) ; exp is ('quote '(a ... ))
           (eval (list 'cons
                       (list 'quote (car text))
                       (list 'quote (cdr text)))
                 env))
          (else text)))) ; exp is ('quote 'abc) or ('quote '())
 
(define (make-quoted text) (list 'quote text))

(define (text-of-quotation exp) (cadr exp))

(define (quoted? exp) (tagged-list? exp 'quote))
