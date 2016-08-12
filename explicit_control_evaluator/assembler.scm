;; If an instruction from the controller text will be executed repeatedly in the
;; course of running a register machine (due to branch or goto instructions that point
;; to the given instruction), we can reduce the number of steps required to run the
;; machine by ascertaining in advance what action constitutes execution of the given
;; instruction, and packaging that action (together with the instruction, for debugging
;; purposes) as an "execution procedure" that can be carried out directly on each
;; repeated call to the instruction -- without having to unpack the meaning of the
;; instruction anew each time. The assembler orchestrates this by replacing every
;; non-label instruction in the controller text with a pair consisting in the
;; instruction together with its execution procedure. With that done, the sequence
;; of execution procedures already contains the information provided by the labels
;; (viz. how to update the program-counter upon finishing the execution procedure)
;; so they may be omitted from the return value.

(define (assembled-instructions assembled-controller)
  (car assembled-controller))

(define (assembled-labels assembled-controller)
  (cadr assembled-controller))

(define (assemble controller machine)
  (let ((result
         (extract-labels controller)))
    (let ((insts
           (car result))
          (labels
           (cdr result)))
      (update-insts! insts labels machine)
      (list insts labels))))

(define (extract-labels text)
  (if (null? text)
      (cons '() '())
      (let ((result
             (extract-labels (cdr text))))
        (let ((insts
               (car result))
              (labels
               (cdr result)))
          (let ((next-inst
                 (car text)))
            (if (symbol? next-inst)
                (if (assoc next-inst labels)
                    (error "Multiply defined label -- EXTRACT-LABEL" next-inst)
                    (cons insts
                          (cons (make-label-entry next-inst insts)
                                labels)))
                (cons (cons (make-instruction next-inst)
                            insts)
                      labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc
         (get-register machine 'pc))
        (flag
         (get-register machine 'flag))
        (operations-table
         (machine 'operations-table)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels
         machine pc flag operations-table)))
     insts)))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (instruction-text inst)
  (car inst))

(define (make-instruction text)
  (cons text '()))

(define (make-label-entry label-name insts)
  (cons label-name insts))
