;;; To do:

;;; rewrite pretty printer to be recursive, and to show all non-false
;;; entries

;;; generate parse tree from chart

;;; index from 0 (maybe)

;;; add probabilities

;; CYK parsing algorithm
;;; Adapted from p. 894 of Russell and Norvig, 2nd Edition

(load "cfgnorm.scm")
(load "bricks-cfg.scm")

;;; for-loop macro
(define-syntax for
  (syntax-rules ()
    [(_ (var start-exp end-exp) body body* ...)
     (let ([end-val end-exp])
       (let loop ([var start-exp])
         (cond
           [(> var end-val) (void)]
           [else
            body
            body* ...
            (loop (add1 var))])))]))

(define cyk-parse
  (lambda (S cfg)
    (cfg-match cfg
      [(,R ,Sigma ,rules ,start-symbol)
       (let ([n (length S)]
             [r (length R)]
             [Rs (list start-symbol)])
         (let ([P (make-chart (add1 n) (add1 n) (add1 r))])
           (for (i 1 n)
             (for-each
               (lambda (prod)
                 (pmatch prod
                   [(,Rj -> ,ai) (guard (eqv? (list-ref S (sub1 i)) ai))
                    (let ([j (var->index Rj R)])
                      (let ([new-value (add-to-list P i 1 j `(,Rj -> ,ai))])
                        (update-chart! P i 1 j new-value)))]
                   [else (void)]))
               rules))
           (for (i 2 n)
             (for (j 1 (add1 (- n i)))
               (for (k 1 (- i 1))
                 (for-each
                   (lambda (prod)
                     (pmatch prod
                       [(,Ra -> ,Rb ,Rc)
                        (let ([A (var->index Ra R)]
                              [B (var->index Rb R)]
                              [C (var->index Rc R)])
                          (let ([val-B (lookup-chart P j k B)]
                                [val-C (lookup-chart P (+ j k) (- i k) C)])
                            (when (and val-B val-C)
                              (let ([new-value
                                     (add-to-list P j i A
                                       `(,Ra -> ,(car val-B) ,(car val-C)))])
                                (update-chart! P j i A new-value)))))]
                       [else (void)]))
                   rules))))
           P
;          (lookup-chart P 1 n (var->index start-symbol R))
           ))])))

(define add-to-list
  (lambda (P i j k X)
    (let ([val (lookup-chart P i j k)])
      (if val (union (list X) val) (list X)))))

(define var->index
  (lambda (X V)
    (let ([M (length V)])
      (let ([M^ (length (memv X V))])
        (add1 (- M M^))))))

;;; make-chart now fills with #f
(define make-chart
  (lambda (n m p)
    (vector-map
     (lambda (x)
       (vector-map
        (lambda (x)
          (make-vector p '#f))
        (make-vector m #f)))
     (make-vector n #f))))

(define lookup-chart
  (lambda (chart n m p)
    (vector-ref (vector-ref (vector-ref chart n) m) p)))

(define update-chart!
  (lambda (chart n m p v)
    (vector-set! (vector-ref (vector-ref chart n) m) p v)))

(define pretty-print-chart
  (lambda (chart)
    (map (lambda (i)
           (map (lambda (j)
                  (let ([ls (filter (lambda (v) v)
                                    (map (lambda (k) (lookup-chart chart j i k))
                                         (cdr (iota (vector-length
                                                     (vector-ref
                                                      (vector-ref chart 0) 0))))))])
                    (if (null? ls) #f ls)))
                (cdr (iota (add1 (- (vector-length chart) i))))))
         (reverse (cdr (iota (vector-length chart)))))))


(define wikipedia-example-grammar
  ; see http://en.wikipedia.org/wiki/CYK_algorithm
  ; with bogus weight goodness!
  (let ([R '((S -> NP VP)
             (VP -> VP PP)
             (VP -> V NP)
             (VP -> eats)
             (PP -> P NP)
             (NP -> Det N)
             (NP -> she)
             (V -> eats)
             (P -> with)
             (N -> fish)
             (N -> fork)
             (Det -> a))])
    (make-cfg
      (S VP PP NP V P N Det) ; V
      (eats she with fork fish a) ; Sigma
      ,R ; rules
      S ; start symbol
      )))

(verify-cnf wikipedia-example-grammar)

(define wikipedia-example-sentence '(she eats a fish with a fork))
(define wiki-answer (cyk-parse wikipedia-example-sentence wikipedia-example-grammar))
(define formatted-wiki-answer (pretty-print-chart wiki-answer))

(define simple-example-sentence '(she eats))
(define simple-answer (cyk-parse simple-example-sentence wikipedia-example-grammar))
(define formatted-simple-answer (pretty-print-chart simple-answer))

(define simple-bricks-sentence-1 '(Dm7 G7 C))
(define simple-bricks-parse-1 (cyk-parse simple-bricks-sentence-1 normalized-bricks-cfg))
(define formatted-simple-bricks-parse-1 (pretty-print-chart simple-bricks-parse-1))

;;; Fake it!  Disregard the keys of the bricks for testing purposes, until we handle it properly
(define fake-simple-bricks-sentence '(Dm7 G7 C F Dm7b5 G7 Cm))
(define fake-simple-bricks-parse (cyk-parse fake-simple-bricks-sentence normalized-bricks-cfg))
(define fake-formatted-simple-bricks-parse (pretty-print-chart fake-simple-bricks-parse))
(define pretty-fake-formatted-simple-bricks-parse (prettify-object fake-formatted-simple-bricks-parse))
(test 'fake-formatted-simple-bricks-parse
  (and (memp (lambda (x) (equal-modulo-genny? x '((Autumn-Leaves-Opening_withoverrun
                  ->
                  (Straight-Approach -> (U.2002 -> Dm7) (U.2003 -> G7))
                  (A.261
                   ->
                   (U.264 -> C)
                   (A.263
                    ->
                    (U.265 -> F)
                    (Sad-Cadence
                     ->
                     (Sad-Approach -> (U.1746 -> Dm7b5) (U.1747 -> G7))
                     (U.1749 -> Cm))))))))  
               (map (lambda (k) (let ([v (lookup-chart fake-simple-bricks-parse 1 7 k)])
                             (let ([pv (prettify-object v)])
                               pv)))
                    (iota (vector-length (vector-ref (vector-ref fake-simple-bricks-parse 1) 7)))))
       #t)
  #t)

#!eof
; Autumn-Leaves-Opening (with overrun)
; (brick Straight-Approach C 1) (chord C 1) (chord F 1) (brick Sad-Cadence A 4)
; (brick Straight-Approach C 1) (chord C 1) (chord F 1) (brick Sad-Cadence A 4)
;(chord Dm7 1) (chord G7 1) (chord C 1) (chord F 1) (chord Bm7b5 1) (chord E7 1) (chord Am 2)
; (chord Dm7 1) (chord G7 1) (chord C 1) (chord F 1) (chord Dm7b5 1) (chord G7 1) (chord Cm 2)

(define simple-bricks-sentence '(Dm7 G7 C F Bm7b5 E7 Am))
(define simple-bricks-parse (cyk-parse simple-bricks-sentence normalized-bricks-cfg))
(define formatted-simple-bricks-parse (pretty-print-chart simple-bricks-parse))
