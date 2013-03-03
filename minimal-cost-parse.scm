(load "prelude.scm")
(load "my-dictionary.scm")

(define brick-cost-ls
;;; Taken from Table 4 of 
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
