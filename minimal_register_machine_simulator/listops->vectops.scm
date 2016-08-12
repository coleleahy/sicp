(define (listops->vectops controller-text)
  (flatmap expand-listops controller-text))

(define (expand-listops inst)
  (cond ((assign-inst? inst)
         (let ((reg-name (assign-reg-name inst))
               (value-exp (assign-value-exp inst)))
           (cond ((operation-exp? value-exp)
                  (let ((operator (operation-exp-op value-exp))
                        (operands (operation-exp-operands value-exp)))
                    (cond ((eq? 'car operator)
                           (set! inst (list
                                       (list 'assign
                                             reg-name
                                             '(op vector-ref)
                                             '(reg the-cars)
                                             (car operands)))))
                          ((eq? 'cdr operator)
                           (set! inst (list
                                       (list 'assign
                                             reg-name
                                             '(op vector-ref)
                                             '(reg the-cdrs)
                                             (car operands)))))
                          ((eq? 'cons operator)
                           (set! inst (list
                                       (list 'perform
                                             '(op vector-set!)
                                             '(reg the-cars)
                                             '(reg free)
                                             (car operands))
                                       (list 'perform
                                             '(op vector-set!)
                                             '(reg the-cdrs)
                                             '(reg free)
                                             (cadr operands))
                                       (list 'assign
                                             reg-name
                                             '(reg free))
                                       (list 'assign
                                             'free
                                             '(op +)
                                             '(reg free)
                                             '(const 1)))))
                          (else
                           (set! inst (list inst))))))
                 (else
                  (set! inst (list inst))))))
        ((perform-inst? inst)
         (let ((action (perform-action inst)))
           (let ((operator (operation-exp-op action))
                 (operands (operation-exp-operands action)))
             (cond ((eq? 'set-car! operator)
                    (set! inst (list
                                (list 'perform
                                      '(op vector-set!)
                                      '(reg the-cars)
                                      (car operands)
                                      (cadr operands)))))
                   ((eq? 'set-cdr! operator)
                    (set! inst (list
                                (list 'perform
                                      '(op vector-set!)
                                      '(reg the-cdrs)
                                      (car operands)
                                      (cadr operands)))))
                   ((eq? 'initialize-stack operator)
                    (set! inst (list 'assign
                                     'the-stack
                                     '(const ()))))
                   (else
                    (set! inst (list inst)))))))
        (else
         (set! inst (list inst))))
  inst)
