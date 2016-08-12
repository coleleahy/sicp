;; Installer of evaluation procedure for unique special form

(define (install-unique) (put uniquely-asserted 'unique 'qeval))

(define (uniquely-asserted pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((extended-frames (qeval (uniquely-asserted-query pattern)
                                   (singleton-stream frame))))
       (if (and (not (stream-null? extended-frames))
                (stream-null? (stream-cdr extended-frames)))
           extended-frames
           the-empty-stream)))
   frame-stream))

(define (uniquely-asserted-query exps) (car exps))
