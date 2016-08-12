;; Execution-procedure constructor
(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine labels operations)
               (make-primitive-exp value-exp machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

;; Syntax interface
(define (assign-value-exp assign-instruction)
  (if (operation-exp? (cddr assign-instruction))
      (cddr assign-instruction)
      (caddr assign-instruction)))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-inst? inst)
  (tagged-list? inst 'assign))
