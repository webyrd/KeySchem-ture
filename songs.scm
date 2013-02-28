;;; Example songs to analyze

(load "prelude.scm")
(load "leadsheet.scm")

;;; expanded, parenthesized leadsheet notation from page 8 of 'Automating the Explantion of
;;; Jazz Chord Progressions using Idiomatic Analysis' by Keller et al.
(define bye-bye-blackbird
  (leadsheet
     (section (measure FM7) (measure Gm7 C7) (measure F6) (measure F6) (measure F6) (measure Abo7) (measure  Gm7) (measure C7))
     (section (measure Gm7) (measure Gm/F$) (measure Gm7/F) (measure C7) (measure Gm7) (measure C7) (measure F6) (measure F6))
     (section (measure F7) (measure E7) (measure Eb7) (measure D7) (measure Gm7 Gm/F$) (measure Gm7/F Gm/A) (measure Gm7) (measure C7))
     (section (measure FM6) (measure Gm7 C7) (measure F6) (measure Am7b5 D7) (measure Gm7) (measure C7) (measure F6) (measure Gm7 C7))))
