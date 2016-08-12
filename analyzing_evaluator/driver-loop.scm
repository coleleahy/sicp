;; Implements a simplified read-eval-print loop for the Scheme language.

(load (list "combination"
            "environments"            
            "eval")) ; loads combinations.scm

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (equal? input '(exit))
        (announce-output "Moriturus te saluto.")
        (let ((output (eval input the-global-environment)))
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

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")
