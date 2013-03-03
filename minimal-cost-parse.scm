(load "prelude.scm")
(load "my-dictionary.scm")

(define brick-cost-ls
;;; Taken from Table 4 of 'Automating the Explanation of Jazz Chord
;;; Progressions using Idiomatic Analysis' by Keller et al.
  '(
;;; specific bricks
    (Cadence . 30)
    (Approach . 45)
    (Dropback . 30)
    (Overrun . 30)
    (Opening . 25)
    (Turnaround . 20)
    (Misc . 40)
;;; generalized bricks
    (On-Off . 1005)
    (Off-On . 1010)
    (On . 550)
;;; missing costs for Invisible, CESH, On-Off+
    ))

(define brick-cost
  (lambda (brick)
    (let ([bt (brick->brick-type brick)])
      (cond
        [(assq bt brick-cost-ls) => cdr]
        [else (error 'brick-name (format "Unknown brick type: ~s" bt))]))))

(define make-mins
  (lambda (n)
    (vector-map
      (lambda (x)
        (vector-map
          (lambda (x)
            #f)
          (make-vector n #f)))
      (make-vector n #f))))

(define get-mins
  (lambda (mins-table n m)
    (vector-ref (vector-ref mins-table n) m)))

(define update-mins!
  (lambda (mins-table n m v)
    (vector-set! (vector-ref mins-table n) m v)))

(define node-cost
  (lambda (n)
    (pmatch n
      [(node ,n)
       (node-cost n)]
      [(node ,n1 ,n2)
       (+ (node-cost n1) (node-cost n2))]
      [,n (brick-cost n)])))

(define find-cheapest-brick
  (lambda (ls)
    (let ([bc-ls (map brick-cost ls)])
      (let ([m (apply min bc-ls)])
        (let ([pos (- (length ls) (length (memv m bc-ls)))])
          (list-ref ls pos))))))

(define minimum-cost-parse
  (lambda (cyk-table)
    (let ([len (vector-length cyk-table)])
      (let ([mins (make-mins len)])
        (for (row 0 (sub1 len))
          (for (col row (sub1 len))
            (let ([nodes (get-chart-bricks cyk-table row col)])
              (let ([cn (find-cheapest-node)])
                (update-mins! mins row col `(node ,cn)))))))
      (let loop ([i (sub1 len)])
        (cond
          [(zero? i) (void)]
          [else
           (for (j (add1 i) (sub1 len))
             (for (k (add1 i) j)
               (let ([n_ij (get-mins mins i j)]
                     [n_ik-1 (get-mins mins i (sub1 k))]
                     [n_kj (get-mins mins k j)])
                 (let ([c_ij (node-cost n_ij)]
                       [c_ik-1 (node-cost c_ik-1)]
                       [c_kj (node-cost c_kj)])
                   (when (< (+ c_ik-1 c_kj) c_ij)
                     (update-mins! mins i j `(node ,n_ik-1 ,n_kj)))))))
           (loop (sub1 i))])))))

(test 'brick-cost-Cadence
  (brick-cost Amen-Cadence)
  30)

(test 'brick-cost-Approach
  (brick-cost Ascending-Minor-CESH-Approach)
  45)

(test 'brick-cost-On-Off
  (brick-cost On-Off-Major-V)
  1005)

;;; Examples from Appendix A of 'Automating the Explanation of Jazz
;;; Chord Progressions using Idiomatic Analysis' by Keller et al.

(test 'brick-cost-1
  (apply + (map brick-cost (list Major-On Extended-Approach)))
  (+ 550 45))

;;; The cost of On-+-Dropback should apparently be 550, which is the cost of 'On'.
;;; Yet the type of On-+-Dropback seems to be Dropback.
;;; Should this be implied by the cost of the Major-On sub-brick?
;(test 'brick-cost-2
;  (apply + (map brick-cost (list On-+-Dropback Straight-Approach)))
;  (+ 550 45))

;;; What is Partial-POT?  Is this in a newer dictionary?
;(test 'brick-cost-3
;  (apply + (map brick-cost (list Partial-POT Minor-On)))
;  (+ 3000 550))
