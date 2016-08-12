;; Constructor
(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing 'off))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'toggle-tracing)
             (if (eq? 'off tracing)
                 (set! tracing 'on)
                 (set! tracing 'off))
             tracing)
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)
               (if (eq? 'on tracing)
                   (begin
                     (display (list 'register-contents name contents))
                     (newline)))))
            (else (error "Unknown request -- REGISTER" message))))
    dispatch))

;; Interface procedures
(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))
