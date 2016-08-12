;; The controller text holds information about the data paths required to implement
;; a machine operated by that controller. The analyze procedure dissects the text
;; and returns a five-element list. The first element is a list of the registers
;; mentioned in the text. The second element is a family of lists, each member of which
;; presents the sources of values assigned to a given register. The third element is a
;; list of registers used to store entry-point labels. The fourth element is a list of
;; registers that are saved to or restored from the stack. The fifth element is a table
;; of the instructions comprising the controller, organized by syntactic type. These
;; five components can inform the design of data paths for a machine that can execute
;; the controller text's instructions.

(define (analyze controller-text)
  (let ((register-names '())
        (registers-and-sources '())
        (entry-point-registers '())
        (save-restore-registers '())
        (assign-insts '())
        (branch-insts '())
        (goto-insts '())
        (perform-insts '())
        (restore-insts '())
        (save-insts '())
        (test-insts '()))
    (define (analyze-instruction inst)
      (cond ((branch-inst? inst)
             (analyze-branch inst))
            ((perform-inst? inst)
             (analyze-perform inst))
            ((test-inst? inst)
             (analyze-test inst))
            ((goto-inst? inst)
             (analyze-goto inst))
            ((save-inst? inst)
             (analyze-save inst))
            ((restore-inst? inst)
             (analyze-restore inst))
            ((assign-inst? inst)
             (analyze-assign inst))))
    (define (make-member element collection)
      (if (member element collection)
          collection
          (cons element collection)))
    (define (analyze-branch inst)
      (set! branch-insts
        (make-member inst branch-insts)))
    (define (analyze-perform inst)
      (set! perform-insts
        (make-member inst perform-insts)))
    (define (analyze-test inst)
      (set! test-insts
        (make-member inst test-insts)))
    (define (analyze-goto inst)
      (set! goto-insts
        (make-member inst goto-insts))
      (let ((destination (goto-dest inst)))
        (if (register-exp? destination)
            (set! register-names
              (make-member (register-exp-reg destination) register-names)))))
    (define (analyze-save inst)
      (set! save-insts
        (make-member inst save-insts))
      (let ((target-reg (save-inst-reg-name inst)))
        (set! register-names
          (make-member target-reg register-names))
        (set! save-restore-registers
          (make-member target-reg save-restore-registers))))
    (define (analyze-restore inst)
      (set! restore-insts
        (make-member inst restore-insts))
      (let ((target-reg (restore-inst-reg-name inst)))
        (set! register-names
          (make-member target-reg register-names))
        (set! save-restore-registers
          (make-member target-reg save-restore-registers))))
    (define (analyze-assign inst)
      (set! assign-insts (make-member inst assign-insts))
      (let ((target-reg (assign-reg-name inst)))
        (set! register-names (make-member target-reg register-names))
        (let ((source (assign-value-exp inst))
              (entry (assoc target-reg registers-and-sources)))
          (if entry
              (set-cdr! entry (make-member source (cdr entry)))
              (set! registers-and-sources (cons (list target-reg source)
                                                registers-and-sources)))
          (cond ((register-exp? source)
                 (set! register-names (make-member (register-exp-reg source)
                                                   register-names)))
                ((operation-exp? source)
                 (for-each
                  (lambda (operand)
                    (if (register-exp? operand)
                        (set! register-names (make-member (register-exp-reg operand)
                                                          register-names))))
                  (operation-exp-operands source)))
                ((label-exp? source)
                 (set! entry-point-registers
                   (make-member target-reg entry-point-registers)))))))
    (for-each analyze-instruction controller-text)
    (list (cons 'register-names register-names)
          (cons 'registers-and-sources registers-and-sources)
          (cons 'entry-point-registers entry-point-registers)
          (cons 'save-restore-registers save-restore-registers)
          (list 'instructions-by-type
                (cons 'assign-insts assign-insts)
                (cons 'branch-insts branch-insts)
                (cons 'goto-insts goto-insts)
                (cons 'perform-insts perform-insts)
                (cons 'restore-insts restore-insts)
                (cons 'save-insts save-insts)
                (cons 'test-insts test-insts)))))

;; Interface procedure
(define (analysis-reg-names controller-analysis)
  (cdr (assoc 'register-names controller-analysis)))
