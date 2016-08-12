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

;; Runs a simplified read-eval-print loop for the object language, based on
;; the global environment defined above.
(driver-loop)
