(define (find-variable exp compile-time-env)
  (define (iter frame-num displacement-num)
    (if (= frame-num (length compile-time-env))
        'not-found
        (let ((the-frame (list-ref compile-time-env frame-num)))
          (if (= displacement-num (length the-frame))
              (iter (+ 1 frame-num) 0)
              (if (eq? exp (list-ref the-frame displacement-num))
                  (list frame-num displacement-num)
                  (iter frame-num (+ 1 displacement-num)))))))
  (iter 0 0))
  

(define (compile-variable exp target linkage compile-time-env)
  (let ((lex-adr
         (find-variable exp compile-time-env)))
    (if (eq? 'not-found lex-adr)
        (end-with-linkage
         linkage
         (make-instruction-sequence
          '() `(,target env)
          `((assign env (op get-global-environment))
            (assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))))
        (end-with-linkage
         linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,lex-adr)
                    (reg env))))))))
