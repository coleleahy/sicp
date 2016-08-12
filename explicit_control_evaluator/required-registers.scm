(define (required-registers controller)
  (define (make-member element collection)
    (if (member element collection)
        collection
        (cons element collection)))
  (let ((register-names '()))
    (define (analyze-instruction inst)
      (cond ((stack-inst? inst)
             (let ((target-reg
                    (stack-inst-reg-name inst)))
               (set! register-names
                 (make-member target-reg register-names))))
            ((goto-inst? inst)
             (let ((destination
                    (goto-dest inst)))
               (if (register-exp? destination)
                   (set! register-names
                     (make-member (register-exp-reg destination)
                                  register-names)))))
            ((assign-inst? inst)
             (let ((target-reg
                    (assign-reg-name inst)))
               (set! register-names
                 (make-member target-reg register-names))
               (let ((source
                      (assign-value-exp inst)))
                 (cond ((register-exp? source)
                        (set! register-names
                          (make-member (register-exp-reg source)
                                       register-names)))
                       ((operation-exp? source)
                        (for-each
                         (lambda (operand)
                           (if (register-exp? operand)
                               (set! register-names
                                 (make-member (register-exp-reg operand)
                                              register-names))))
                         (operation-exp-operands source)))))))))
    (for-each analyze-instruction controller)
    register-names))
