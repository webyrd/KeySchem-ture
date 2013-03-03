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

(define make-chart
  (lambda (n m p)
    (vector-map
      (lambda (x)
        (vector-map
          (lambda (x)
            (make-vector p #f))
          (make-vector m #f)))
      (make-vector n #f))))

(define lookup-chart
  (lambda (chart n m p)
    (vector-ref (vector-ref (vector-ref chart n) m) p)))

(define get-chart-bricks
  (lambda (chart n m)
    (filter (lambda (x) x) (vector->list (vector-ref (vector-ref chart n) m)))))

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
