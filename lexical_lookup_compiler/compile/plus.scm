;; Illustrates how to open-code a primitive procedure of the source language in terms
;; of a primitive register-machine operation. Open-coding of primitives increases
;; the efficiency of the output code by eliminating instructions that lookup the
;; procedure name in the environment, assign the result to the proc register, build
;; the list of argument values in the argl register, test whether the procedure is
;; primitive or compound, and branch accordingly. Instead, the output code will simply
;; contain instructions to compile the operands into dedicated registers (arg1 and
;; arg2) and to apply the register machine's built-in operation to those registers.
;; An extra complication arises, however, when we consider the possibility that the
;; source-language program might redefine a variable that typically designates a
;; primitive procedure. In that case, we want the compiler *not* to open-code the
;; program's calls the the function named by that variable, since the variable no
;; longer refers to the usual primitive procedure. To achieve this behavior, the
;; compiler consults the compile-time-environment to see whether the procedure name
;; has been redefined, and only proceeds to output the open-coded instructions in
;; case the answer is negative.

(define (spread-arguments operands compile-time-env)
  (preserving
   '(arg1 arg2)
   (compile (car operands) 'arg1 'next compile-time-env)
   (compile (cadr operands) 'arg2 'next compile-time-env)))

(define (plus? exp)
  (tagged-list? exp '+))

(define (compile-plus exp target linkage compile-time-env)
  (let ((lex-adr
         (find-variable '+ compile-time-env)))
    (cond ((eq? 'not-found lex-adr)
           (let ((operands (cdr exp)))
             (append-instruction-sequences
              (spread-arguments operands compile-time-env)
              (end-with-linkage
               linkage
               (make-instruction-sequence
                '(arg1 arg2) (list target)
                `((assign ,target (op +) (reg arg1) (reg arg2))))))))
          (else
           (compile-application exp target linkage compile-time-env)))))
