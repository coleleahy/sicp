;; Illustrates how to open-code a primitive procedure of the source language in terms
;; of a primitive register-machine operation. Open-coding of primitives increases
;; the efficiency of the output code by eliminating instructions that lookup the
;; procedure name in the environment, assign the result to the proc register, build
;; the list of argument values in the argl register, test whether the procedure is
;; primitive or compound, and branch accordingly. Instead, the output code will simply
;; contain instructions to compile the operands into dedicated registers (arg1 and
;; arg2) and to apply the register machine's built-in operation to those registers.

(define (spread-arguments operands)
  (preserving
   '(arg1 arg2)
   (compile (car operands) 'arg1 'next)
   (compile (cadr operands) 'arg2 'next)))

(define (plus? exp)
  (tagged-list? exp '+))

(define (compile-plus exp target linkage)
  (let ((operands (cdr exp)))
    (append-instruction-sequences
     (spread-arguments operands)
     (end-with-linkage
      linkage
      (make-instruction-sequence
       '(arg1 arg2) (list target)
       `((assign ,target (op +) (reg arg1) (reg arg2))))))))
