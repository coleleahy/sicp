;; Implements a simplified read-eval-print loop for the Scheme language.

;; N.B. The primitive procedure *read* waits for the user to hit RET after typing a
;; complete sexp (e1 ... ) and then returns the list designating that sexp, viz.
;; (quote (e1 ... )), viz. (list (quote e1) ... ). For example, if the user types the
;; opening parenthesis followed by the plus sign '+' followed by a space followed by
;; the numeral '2' followed by a space followed by the numeral '6' followed by the
;; closing parenthesis, then the interpreter will return the list whose first element
;; is (quote +) and whose second and third elements are (quote 2) and (quote 6). It
;; is important to note that the interpreter classifies (quote 2) as a number and not
;; a symbol. Likewise, (quote "abc") is classified as a string and not a symbol.

(load (list "combination"
            "environments"            
            "eval"
            "thunk"))

;; Quotes the expression typed by the user and computes its actual value in the global
;; environment. If the quoted expression evaluates to a non-thunk, the return value is
;; the result of the evaluation. If the quoted expression evaluates to a thunk, the
;; return value is the result of forcing that thunk. If the quoted expression evaluates
;; to an already forced thunk, the return value is the memoized result of the forcing
;; that already occurred.
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (equal? input '(exit))
        (announce-output "Moriturus te saluto.")
        (let ((output
               (actual-value input the-global-environment)))
          (announce-output output-prompt)
          (user-print output)
          (driver-loop)))))

;; Designed to avoid printing a long (and possibly cyclical) environment component
;; when displaying the value of a compound-procedure name.
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '*environment-component*))
      (display object)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define input-prompt ";;; L-Eval input:")

(define output-prompt ";;; L-Eval value:")
