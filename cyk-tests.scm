(load "cyk.scm")

(test 'make-chart-1
  (make-chart 2 3 4)
  '#(#(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f #f))
     #(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f #f))))

(test 'update-chart-1
  (let ((c (make-chart 2 3 4)))
    (update-chart! c 1 2 3 'success)
    c)
  '#(#(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f #f))
     #(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f success))))

(test 'lookup-chart-1
  (let ((c (make-chart 2 3 4)))
    (update-chart! c 1 2 3 'success)
    (lookup-chart c 1 2 3))
  'success)

(test 'lookup-chart-2
  (let ((c (make-chart 2 3 4)))
    (update-chart! c 1 2 3 'success)
    (lookup-chart c 1 0 3))
  #f)

(test 'var->index
  (var->index 'C '(A B C D E F G))
  3)



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

(define blue-moon-beginning
;;; First four measures, as shown in parse chart in Appendix A of
;;; 'Automating the Explanation of Jazz Chord Progressions using
;;; Idiomatic Analysis' by Keller et al.
  '(C6 Am7 Dm7 G7 C6 Am7 Dm7 G7))
(define blue-moon-beginning-parse (cyk-parse blue-moon-beginning normalized-bricks-cfg))
(define formatted-blue-moon-beginning-parse (pretty-print-chart blue-moon-beginning-parse))
(define pretty-formatted-blue-moon-beginning-parse (prettify-object formatted-blue-moon-beginning-parse))
