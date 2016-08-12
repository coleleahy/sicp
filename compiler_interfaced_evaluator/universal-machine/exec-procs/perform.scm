;; Execution-procedure constructor
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

;; Syntax interface
(define (perform-action inst) (cdr inst))

(define (perform-inst? inst)
  (tagged-list? inst 'perform))
