;; Procedures to convert back and forth between symbols beginning with question mark
;; and lists representing those symbols in easily manipulated format

(define (optimize-syntax exp)
  (map-over-symbols detach-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))  
        (else exp)))                

(define (detach-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?                
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))                      

(define (attach-question-mark variable) ; variable has form '(? 7 x) or '(? x)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable)) 
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))
