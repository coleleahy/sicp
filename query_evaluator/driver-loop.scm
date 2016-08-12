;; Implements a read-eval-print loop for a simple database-query language.

;; Note that while the read procedure will store (list 'a 'b) when the user types
;; "(a b)," it will store (cons 'a 'b) when the user types "(a . b)." A good mnemonic:
;; to store (list 'a 'b) type what the interpreter would have printed as the value
;; you had typed "(list 'a 'b)," namely "(a b)," and to store (cons 'a 'b) type what
;; the interpreter would have printed as the value if you had typed "(cons 'a 'b),"
;; namely "(a . b)."

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (optimize-syntax (read))))
    (cond ((equal? q '(exit))
           (exit))
          ((assertion-to-add? q)
           (add-rule-or-assertion! (assertion-to-add-body q))
           (newline)
           (display "Assertion added to database.")
           (driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (attach-question-mark v))))
             (qeval q (singleton-stream '()))))
           (driver-loop)))))

(define (announce-output string)
  (newline) (display string) (newline))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define output-prompt ";;; Query results:")

(define input-prompt ";;; Query input:")               
