;; Execution-procedure constructor
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

;; Syntax interface
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (test-inst? inst)
  (tagged-list? inst 'test))
