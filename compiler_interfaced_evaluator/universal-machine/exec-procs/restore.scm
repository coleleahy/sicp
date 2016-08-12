;; Execution-procedure constructor
(define (make-restore inst machine pc)
  (let ((reg-name (restore-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name))
          (stack (get-register-stack machine reg-name)))
      (lambda ()
        (set-contents! reg (pop stack))
        (advance-pc pc)))))

;; Syntax interface
(define (restore-inst-reg-name restore-instruction)
  (cadr restore-instruction))

(define (restore-inst? inst)
  (tagged-list? inst 'restore))
