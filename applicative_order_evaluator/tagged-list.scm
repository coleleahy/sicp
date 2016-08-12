;; Checks the syntactic type of an expression against a given type tag

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
