;; Constructors and selectors for table of values stored under arbitrarily many keys

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup table keys)
      (let ((subtable (assoc (car keys) (cdr table))))
        (cond ((and subtable (null? (cdr keys)))
               (cdr subtable))
              ((and subtable (pair? (cdr keys)))
               (lookup subtable (cdr keys)))
              (else false))))
    (define (insert! table value keys)
      (let ((subtable (assoc (car keys) (cdr table))))
        (cond ((and subtable (null? (cdr keys)))
               (set-cdr! subtable value))
              ((and subtable (pair? (cdr keys)))
               (insert! subtable value (cdr keys)))
              ((and (not subtable) (null? (cdr keys)))
               (set-cdr! table
                         (cons (cons (car keys) value)
                               (cdr table))))
              (else
               (let ((new-subtable (list (car keys))))
                 (insert! new-subtable value (cdr keys))
                 (set-cdr! table
                           (cons new-subtable
                                 (cdr table))))))))
    (define (table-print) local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup)
             (lambda (x) (lookup local-table x)))
            ((eq? m 'insert!)
             (lambda (x y) (insert! local-table x y)))
            ((eq? m 'print) (table-print))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define table (make-table))

(define (put value . keys)
  ((table 'insert!) value keys))

(define (get . keys)
  ((table 'lookup) keys))

(define (print) (table 'print))
