(define (stackops->listops controller-text)
  (flatmap expand-stackops controller-text))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate operator initial sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator initial (cdr sequence)))))

(define (expand-stackops inst)
  (cond ((save-inst? inst)
         (let ((reg-name (stack-inst-reg-name inst)))
           (set! inst (list
                       (list 'assign
                             'the-stack
                             '(op cons)
                             (list 'reg reg-name)
                             '(reg the-stack))))))
        ((restore-inst? inst)
         (let ((reg-name (stack-inst-reg-name inst)))
           (set! inst (list
                       (list 'assign
                             reg-name
                             '(op car)
                             '(reg the-stack))
                       (list 'assign
                             'the-stack
                             '(op cdr)
                             '(reg the-stack))))))
        (else (set! inst (list inst))))
  inst)

(define (stack-inst-reg-name stack-inst)
  (cadr stack-inst))

(define (save-inst? inst)
  (tagged-list? inst 'save))

(define (restore-inst? inst)
  (tagged-list? inst 'restore))
