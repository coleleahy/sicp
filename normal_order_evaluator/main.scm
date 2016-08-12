;; Runs a simplified read-eval-print loop for the Scheme language.

;; Aliases the metalanguage's apply procedure so that it can be accessed
;; even after the variable 'apply' is redefined in combination.scm. The
;; definition below must be evaluated before loading driver-loop.scm, which
;; in turn loads combination.scm in its chain of dependencies.
(define apply-in-underlying-scheme apply)

(load (list "driver-loop"
            "environments.scm"))

;; Defines the global environment as an extension of the empty environment
;; in which the object language's primitive-function names etc. are bound
;; to the appropriate values.
(define the-global-environment (setup-environment))

;; Evaluating the following expressions installs cons, car, and cdr as compound
;; procedures, which are therefore non-strict in their arguments. This affords us
;; the ability to construct "lazy lists" with the non-strict cons operation, thus
;; obviating the need for streams as a separate data type for the handling of
;; infinite sequences.
(eval '(define (cons x y)
         (lambda (m) (m x y)))
      the-global-environment)
(eval '(define (car z)
         (z (lambda (p q) p)))
      the-global-environment)
(eval '(define (cdr z)
         (z (lambda (p q) q)))
      the-global-environment)

;; Runs a simplified read-eval-print loop for the object language, based on
;; the global environment defined above.
(driver-loop)
