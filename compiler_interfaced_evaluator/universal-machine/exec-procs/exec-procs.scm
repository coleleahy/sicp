(define (make-execution-procedure inst labels machine pc flag ops)
  (cond ((assign-inst? inst)
         (make-assign inst machine labels ops pc))
        ((test-inst? inst)
         (make-test inst machine labels ops flag pc))
        ((branch-inst? inst)
         (make-branch inst machine labels flag pc))
        ((goto-inst? inst)
         (make-goto inst machine labels pc))
        ((save-inst? inst)
         (make-save inst machine pc))
        ((restore-inst? inst)
         (make-restore inst machine pc))
        ((perform-inst? inst)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;; Execution-procedure constructors
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs (map
                 (lambda (operand)
                   ;; (if (label-exp? operand)
                   ;;     (error "Cannot apply operation to label -- ASSEMBLE"
                   ;;            (list (operation-exp-op exp) operand))
                   ;;     (make-primitive-exp operand machine labels)))
                   (make-primitive-exp operand machine labels))
                 (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (lambda () (constant-exp-value exp)))
        ((label-exp? exp)
         (lambda () (lookup-label labels (label-exp-label exp))))
        ((register-exp? exp)
         (lambda () (get-contents (get-register machine (register-exp-reg exp)))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (lookup-label labels label-name)
    (let ((val (assoc label-name labels)))
      (if val
          (cdr val)
          (error "Undefined label -- ASSEMBLE" label-name))))
;; "Increments" the program counter
(define (advance-pc pc)
    (set-contents! pc (cdr (get-contents pc))))

;; Syntax interface
(define (stack-inst-reg-name inst) (cadr inst))

(define (stack-inst? inst)
  (or (save-inst? inst)
      (restore-inst? inst)))

(define (operation-exp-operands operation-exp) (cdr operation-exp))

(define (operation-exp-op operation-exp) (cadr (car operation-exp)))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (constant-exp-value exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (label-exp-label exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (register-exp-reg exp) (cadr exp))

(define (register-exp? exp) (tagged-list? exp 'reg))
