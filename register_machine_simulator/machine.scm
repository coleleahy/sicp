;; Define a controller text. Give an interpretation of the text's operation symbols.
;; Make a machine from the text and the specified interpretation. Set the contents of
;; the input registers. Start the machine. Get the contents of the output registers.

(load (list "registers"
            "stacks"
            "analyzer"
            "assembler"
            "exec-procs"
            "exec-proc-assign"
            "exec-proc-test"
            "exec-proc-branch"
            "exec-proc-goto"            
            "exec-proc-restore"
            "exec-proc-save"
            "exec-proc-perform"))

(define (make-machine ops ctrl)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-controller) ctrl)
    machine))

(define (make-new-machine)
  (let* ((inst-counter 0)
         (inst-tracing 'off)
         (label-tracing 'off)
         (labels '*waiting-for-input*)
         (controller-analysis '*waiting-for-input*)
         (instruction-sequence '*waiting-for-input*)
         (pc (make-register 'pc))
         (flag (make-register 'flag))
         (stack-table '())
         (register-table
          (list (list 'pc pc)
                (list 'flag flag)))
         (operations-table
          (list (list '+ +)
                (list '- -)
                (list '* *)
                (list '/ /)
                (list '= =)
                (list '< <)
                (list '<= <=)
                (list '> >)
                (list '>= >=)
                (list 'read read)
                (list 'print
                      (lambda (arg)
                        (display arg) (newline)))
                (list 'initialize-inst-counter
                      (lambda ()
                        (set! inst-counter 0)))
                (list 'print-inst-counter
                      (lambda ()
                        (display (list 'inst-counter '= inst-counter))
                        (newline)))
                (list 'initialize-stacks
                      (lambda ()
                        (for-each
                         (lambda (stack)
                           ((cadr stack) 'initialize))
                         stack-table)))
                (list 'print-stack-statistics
                      (lambda ()
                        (for-each
                         (lambda (stack)
                           (((cadr stack) 'print-statistics)
                            (car stack)))
                         stack-table))))))
    (define (allocate-register name)
      (if (assoc name register-table)
          (error "Multiply defined register:" name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
      'register-allocated)
    (define (allocate-stack name)
      (if (assoc name stack-table)
          (error "Multiply defined stack:" name)
          (set! stack-table
            (cons (list name (make-stack)) stack-table)))
      'stack-allocated)
    (define (get-register name)
      (let ((val (assoc name register-table)))
        (if val
            (cadr val)
            (error "Unknown register:" name))))
    (define (get-stack name)
      (let ((val (assoc name stack-table)))
        (if val
            (cadr val)
            (error "Unknown stack:" name))))
    (define (execute)
      (let ((insts (get-contents pc)))
        (if (null? insts)
            'done
            (begin
              (if (eq? 'on label-tracing)
                  (let ((preceding-labels
                         (map car
                              (filter
                               (lambda (lbl) (equal? (cdr lbl) insts))
                               labels))))
                    (if (pair? preceding-labels)
                        (for-each pp preceding-labels))))
              (let ((inst (car insts)))
                (if (eq? 'on inst-tracing)
                    (pp (instruction-text inst)))
                ((instruction-execution-proc inst))
                (set! inst-counter (+ 1 inst-counter)))
              (execute)))))
    (define (dispatch message)
      (cond ((eq? message 'controller-analysis) controller-analysis)
            ((eq? message 'operations-table) operations-table) ; used in assembler
            ((eq? message 'get-register) get-register) ; used below
            ((eq? message 'get-stack) get-stack)       ; used below
            ((eq? message 'toggle-label-tracing)
             (if (eq? 'off label-tracing)
                 (set! label-tracing 'on)
                 (set! label-tracing 'of))
             label-tracing)
            ((eq? message 'toggle-inst-tracing)
             (if (eq? 'off inst-tracing)
                 (set! inst-tracing 'on)
                 (set! inst-tracing 'off))
             inst-tracing)
            ((eq? message 'start)
             (set-contents! pc instruction-sequence)
             (execute))
            ((eq? message 'install-operations)
             (lambda (ops)
               (set! operations-table (append ops operations-table))))
            ((eq? message 'install-controller)
             (lambda (ctrl)
               (set! controller-analysis (analyze ctrl))
               (for-each
                (lambda (reg-name)
                  (allocate-register reg-name)
                  (allocate-stack reg-name))
                (analysis-reg-names controller-analysis))
               (let ((assembled-ctrl (assemble ctrl dispatch)))
                 (set! instruction-sequence (car assembled-ctrl))
                 (set! labels (cadr assembled-ctrl)))))
            (else (error "Unknown request -- MACHINE" message))))
    dispatch))

;; Convenient interface procedures
(define (start machine)
  (machine 'start))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (get-register-contents machine reg-name)
  (get-contents (get-register machine reg-name)))

(define (set-register-contents! machine reg-name val)
  (set-contents! (get-register machine reg-name) val)
  'done)

(define (toggle-register-tracing! machine reg-name)
  ((get-register machine reg-name) 'toggle-tracing))

(define (get-stack machine reg-name)
  ((machine 'get-stack) reg-name))
