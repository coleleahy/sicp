;; Runs a read-eval-print loop for a simple logic-programming query language

(load (list "always-true"
            "and"
            "constant-variable"
            "database" ; does anything execute on load?
            "driver-loop"
            "frame"
            "lisp-value"
            "not"
            "or"
            "qeval-simple"
            "qeval"
            "query"
            "question-mark"
            "rule-assertion"
            "stream"
            "table"
            "tagged-list"))

(make-table)

(install-always-true)
(install-and)
(install-lisp-value)
(install-not)
(install-or)

(driver-loop)
