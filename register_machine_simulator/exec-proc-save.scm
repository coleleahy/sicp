;; Execution-procedure constructor
(define (make-save inst machine pc)
  (let ((reg-name (save-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name))
          (stack (get-stack machine reg-name)))
      (lambda ()
        (push stack (get-contents reg))
        (advance-pc pc)))))

;; Syntax interface
(define (save-inst-reg-name save-instruction)
  (cadr save-instruction))

(define (save-inst? inst)
  (tagged-list? inst 'save))
