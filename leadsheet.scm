;;; Scheme-ified lead sheet notation, a la Impro-Visor

(define-syntax leadsheet
  (syntax-rules (section ! /)
    [(_ (section chord* ...) ...)
     (quote ((section chord* ...) ...))]))
