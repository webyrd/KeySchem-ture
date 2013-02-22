;;; Trivial leadsheet macro.  In the future may want to more closely
;;; match the leadsheet notation in the Impro-Visor papers, or have a
;;; variant without 'section' annotations.
(define-syntax leadsheet
  (syntax-rules (section measure)
    [(_ (section chord* ...) ...)
     (quote (leadsheet (section chord* ...) ...))]))
