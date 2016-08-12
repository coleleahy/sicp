;; Installer of evaluation procedure for always-true special form

(define (install-always-true) (put always-true 'always-true 'qeval))

(define (always-true ignore frame-stream) frame-stream)
