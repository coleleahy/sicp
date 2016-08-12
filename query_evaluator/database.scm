;; Constructors and selectors for database of rules and assertions

;; Regard the database as a specification of the atomic theory of a model, obtained
;; by closing the assertions under the rules. The atomic theory determines the
;; propositional theory of the model, and in particular the negation of an atomic
;; sentence belongs to the propositional theory just in case its negatum does not
;; belong to the atomic theory.

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put (cons-stream assertion
                            current-assertion-stream)
               key
               'assertion-stream)))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put (cons-stream rule
                              current-rule-stream)
                 key
                 'rule-stream))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-all-rules) THE-RULES)

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define THE-ASSERTIONS the-empty-stream)

(define THE-RULES the-empty-stream)
