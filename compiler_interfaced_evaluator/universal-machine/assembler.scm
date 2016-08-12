;; If an instruction from the program text will be executed repeatedly in the
;; course of running a register machine (due to branch or goto instructions that point
;; to the given instruction), we can reduce the number of steps required to run the
;; machine by ascertaining in advance what action constitutes execution of the given
;; instruction, and packaging that action (together with the instruction, for debugging
;; purposes) as an "execution procedure" that can be carried out directly on each
;; repeated call to the instruction -- without having to unpack the meaning of the
;; instruction anew each time. The assembler orchestrates this by replacing every
;; non-label instruction in the program text with a pair consisting in the
;; instruction together with its execution procedure. With that done, the sequence
;; of execution procedures already contains the information provided by the labels
;; (viz. how to update the program-counter upon finishing the execution procedure)
;; so they may be omitted from the return value.

(define (make-instruction text)
  (cons text 'awaiting-exec-proc))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (label-instructions label)
  (cdr label))

(define (extract-labels text)
  (if (null? text)
      (list '() '())
      (let* ((result (extract-labels (cdr text)))
             (insts (car result))
             (labels (cadr result))
             (this-text (car text)))
        (if (symbol? this-text)
            (if (assoc this-text labels)
                (error "Multiply defined label -- EXTRACT-LABEL" this-text)
                (list insts
                      (cons (make-label-entry this-text insts)
                            labels)))
            (list (cons (make-instruction this-text)
                        insts)
                  labels)))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (operations-table (machine 'operations-table)))
    (for-each (lambda (inst)
                (set-instruction-execution-proc!
                 inst
                 (make-execution-procedure
                  (instruction-text inst) labels
                  machine pc flag operations-table)))
              insts)))

(define (assemble program machine)
  (let* ((result (extract-labels program))
         (insts (car result))
         (labels (cadr result)))
    (update-insts! insts labels machine) ; installs an exec-proc in each inst
    (list insts labels)))

(define (instructions-with-exec-procs assembled-program)
  (car assembled-program))

(define (labels-with-entry-locations assembled-program)
  (cadr assembled-program))
