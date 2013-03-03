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
