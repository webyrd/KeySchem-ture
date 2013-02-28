(load "prelude.scm")
(load "my-dictionary.scm")
(load "cfgnorm.scm")

#|
; the new hotness
(define brick-rules
  `((,GenMinor -> ,GenMinor_var1)
    (,GenMinor_var1 -> Cm)
    (,GenMinor_var2 -> Cm7)
    (,GenDom -> ,GenDom_var1)
    (,GenDom_var1 -> G7)
    (,GenDom_var2 -> ,GenII G7)))

(define prettify-rules
  (lambda (rules)
    (let loop ([rules rules][acc '()])
      (pmatch rules
        [() (reverse acc)]
        [((,bn -> . ,rhs*) . ,rest)
         (let ([rule `(,(prettify-object bn) -> . ,(map prettify-object rhs*))])
           (loop rest (cons rule acc)))]))))


;(define rules (append brick-rules S-rules))
;(define brick-names (map (lambda (r) (brick->brick-name (car r))) brick-rules))

;;; change to just include symbols, not bricks
(define chords
  (let ([blocks (map cddr rules)])
    (let ([blocks (map remv-dups blocks)])
      (difference (apply union blocks) brick-names))))
|#

(define tmp-chords-for-hax
  '(Em7b5 Co7 Gm C+ Cm+ AbM7$5 DbM7 D/C Db/C C/E D$o Dbm F/G
          Dbm7 AbM7 Cm7/Bb Am/C Dm7/G A7$11 G7+ Bm G/B C7$11 B+ G A7/E
          Ebdim F$7 G7/D F7$11 Am7b5 Dm7b5 C$m7b5 Em Am G7/F Ebm7 Bbm7
          Eb7 F$m7b5 C$o G/D C7$4 Gm7/C G7sus Ebm7b5 C/G Ebo Dm Bm7b5
          D7 FM7 B7 Db7 C7/Bb Dbdim Fm7 Cm CmM7 Cm6 Cm7 G7 Fm Gsus GM7
          Am7 Bm7 F$o Bo7 F7_ Em7 Dm7 Bb Eb Ab7 Abm7 Gb7 F7 EM7 EbM7
          E7 F Abm Fm7b5 C C6 Em7b5/Bb A7 Gm7 Gm/F$ Gm7/F C7 Bb7sus4
          Bb7))

(define hax-enabled
  (lambda (rules)
    (let loop ([rules rules][acc '()])
      (cond
        [(null? rules) (reverse acc)]
        [else (loop (cdr rules)
                    (cons (map (lambda (x)
                                 (if (or (eq? x '->)
                                         (memq x tmp-chords-for-hax))
                                     x
                                     (list 'unquote x)))
                               (car rules))
                          acc))]))))

(define brick-rules
  `((,GenMinor -> ,GenMinor_var1)
    (,GenMinor_var1 -> Cm)
    (,GenMinor_var2 -> Cm7)
    (,GenDom -> ,GenDom_var1)
    (,GenDom_var1 -> G7)
    (,GenDom_var2 -> ,GenII G7)
    (,GenII -> ,GenII_var1)
    (,GenII_var1 -> Dm7)
    (,GenII_var2 -> Dm7b5)
    (,LiberalII -> ,LiberalII_var1)
    (,LiberalII_var1 -> ,GenII)
    (,LiberalII_var2 -> D7)
    (,GenIII -> ,GenIII_var1)
    (,GenIII_var1 -> Em7)
    (,GenIII_var2 -> E7)
    (,GenIII_var3 -> Em7b5)
    (,GenVI -> ,GenVI_var1)
    (,GenVI_var1 -> Am7)
    (,GenVI_var2 -> A7)
    (,GenVI_var3 -> Am7b5)
    (,Gen7 -> ,Gen7_var1)
    (,Gen7_var1 -> C7)
    (,Gen7_var2 -> Cm7)
    (,An-Approach -> ,GenII G7)
    (,Liberal-Approach -> ,Liberal-Approach_var1)
    (,Liberal-Approach_var1 -> ,An-Approach)
    (,Liberal-Approach_var2 -> ,GenII Gm7)
    (,SuperGenDom -> ,SuperGenDom_var1)
    (,SuperGenDom_var1 -> G7)
    (,SuperGenDom_var2 -> ,GenII G7)
    (,SuperGenDom_var3 -> ,GenII Db7)
    (,GenMajorTonic -> ,GenMajorTonic_main)
    (,GenMajorTonic_main -> C)
    (,GenMajorTonic_iii -> Em7)
    (,Off -> ,Off_dominant)
    (,Off_dominant -> C7)
    (,Off_major -> C)
    (,Off_minor -> Cm)
    (,Off_minor7 -> Cm7)
    (,Off_minor-major -> CmM7)
    (,Off_diminished -> Co7)
    (,TonicOrDom -> ,TonicOrDom_major)
    (,TonicOrDom_major -> C)
    (,TonicOrDom_minor -> Cm)
    (,TonicOrDom_dom -> C7)
    (,MajorOrDom -> ,MajorOrDom_major)
    (,MajorOrDom_major -> C)
    (,MajorOrDom_dom -> C7)
    (,DomOrMinor -> ,DomOrMinor_minor)
    (,DomOrMinor_minor -> Gm)
    (,DomOrMinor_minor7 -> Gm7)
    (,DomOrMinor_dom -> G7)
    (,Ascending-Minor-CESH -> ,Ascending-Minor-CESH_3step)
    (,Ascending-Minor-CESH_3step -> Cm Cm+ Cm6)
    (,Ascending-Minor-CESH_4step -> Cm Cm+ Cm6 Cm7)
    (,Ascending-Minor-CESH-Approach -> Cm Cm+ Cm6 C7)
    (,Descending-Minor-CESH -> ,Descending-Minor-CESH_3step)
    (,Descending-Minor-CESH_3step -> Cm CmM7 Cm7)
    (,Descending-Minor-CESH_4step
     ->
     ,Descending-Minor-CESH_3step
     Cm6)
    (,Ascending-Major-CESH -> ,Ascending-Major-CESH_3step)
    (,Ascending-Major-CESH_3step -> C C+ C6)
    (,Ascending-Major-CESH_4step -> C C+ C6 C7)
    (,Ascending-&-Descending-Minor-CESH
     ->
     ,Ascending-&-Descending-Minor-CESH_4step)
    (,Ascending-&-Descending-Minor-CESH_4step -> Cm Cm+ Cm6 Cm+)
    (,Ascending-&-Descending-Minor-CESH_5step -> Cm Cm+ Cm6 Cm7
                                              Cm6)
    (,Ascending-&-Descending-Major-CESH -> ,Major-On AbM7$5
                                        ,Major-On AbM7$5 AbM7)
    (,Seven-Chord-Dropback -> ,Seven-Chord-Dropback_main)
    (,Seven-Chord-Dropback_main -> ,Dropback-Approach C)
    (,Seven-Chord-Dropback_minoron
     ->
     ,Minor-On
     ,Perfect-Cadence
     ,Long-Cadence)
    (,Seven-Chord-Dropback_rainy -> Dm7 G7 C Ebo Dm7 G7 C)
    (,Amen-Cadence -> ,TonicOrDom C)
    (,Autumn-Leaves-Opening -> ,Autumn-Leaves-Opening_main)
    (,Autumn-Leaves-Opening_main
     ->
     ,Straight-Approach
     C
     ,Sad-Cadence)
    (,Autumn-Leaves-Opening_withoverrun -> ,Straight-Approach C
                                        F ,Sad-Cadence)
    (,Autumnal-Approach -> ,Autumnal-Approach_var1)
    (,Autumnal-Approach_var1 -> ,An-Approach ,An-Approach)
    (,Autumnal-Approach_var2 -> ,Minor-On ,An-Approach)
    (,Autumnal-Cadence -> ,Autumnal-Cadence_main)
    (,Autumnal-Cadence_main -> ,Autumnal-Approach ,GenMinor)
    (,Autumnal-Cadence_var1 -> ,GenII ,Sad-Cadence)
    (,Autumnal-Cadence_var2 -> ,GenMinor ,Cadence)
    (,Bemsha-Turnaround-1 -> C Eb7 D7 Db7)
    (,Bemsha-Turnaround-2 -> C Ab7 DbM7 Gb7)
    (,Bemsha-Turnaround-3 -> C Bb7 EbM7 Ab7)
    (,Body-&-Soul-Approach -> ,Body-&-Soul-Approach_main)
    (,Body-&-Soul-Approach_main -> Dm7 ,GenDom ,An-Approach)
    (,Body-&-Soul-Approach_var1 -> Dm A7 Ab7 ,GenDom)
    (,Body-&-Soul-Approach_var2
     ->
     ,On-Off-Minor-Somewhere
     ,Straight-Approach)
    (,Body-&-Soul-Cadence -> ,Body-&-Soul-Cadence_major)
    (,Body-&-Soul-Cadence_major -> ,Body-&-Soul-Approach C)
    (,Body-&-Soul-Cadence_minor -> ,Body-&-Soul-Approach Cm)
    (,Chromatic-Dropback -> ,Chromatic-Dropback_main)
    (,Chromatic-Dropback_main -> C B7 Bb7 A7)
    (,Chromatic-Major-Walkup -> ,Major-On ,On-Off-Major-bII)
    (,Chromatic-Minor-Ascending
     ->
     ,Chromatic-Minor-Ascending_3steps)
    (,Chromatic-Minor-Ascending_3steps -> ,GenII ,GenII ,GenII)
    (,Chromatic-Minor-Ascending_4steps
     ->
     ,Chromatic-Minor-Ascending_3steps
     ,GenII)
    (,Chromatic-Minor-Descending
     ->
     ,Chromatic-Minor-Descending_3steps)
    (,Chromatic-Minor-Descending_3steps -> Dm7 Dbm7 Cm7)
    (,Chromatic-Minor-Descending_4steps
     ->
     ,Chromatic-Minor-Descending_3steps
     Cm7)
    (,Chromatically-Descending-Dominants
     ->
     ,Chromatically-Descending-Dominants_3steps)
    (,Chromatically-Descending-Dominants_3steps -> A7 Ab7 G7)
    (,Chromatically-Descending-Dominants_4steps
     ->
     Bb7
     ,Chromatically-Descending-Dominants_3steps)
    (,Chromatically-Descending-Dominants_5steps
     ->
     B7
     ,Chromatically-Descending-Dominants_4steps)
    (,Chromatically-Descending-Dominants_6steps
     ->
     C7
     ,Chromatically-Descending-Dominants_5steps)
    (,Coltrane-Cadence -> Dm7 ,Perfect-Cadence ,Perfect-Cadence
                       ,Perfect-Cadence)
    (,Cyclic-Approach -> ,Cyclic-Approach_2)
    (,Cyclic-Approach_2 -> Dm7 D7 Gm7 G7)
    (,Cyclic-Approach_3 -> ,Cyclic-Approach_2 Gm7 G7)
    (,Diatonic-Walkup -> ,Diatonic-Walkup_3stepsfromii)
    (,Diatonic-Walkup_3stepsfromii -> Dm7 Em7 Fm7)
    (,Diatonic-Walkup_4stepsfromii -> Dm7 Em7 Fm7 Gb7)
    (,Diatonic-I-ii-iii-ii -> C Dm7 Em7 Dm7)
    (,Diatonic-I-ii-iii-IV -> C Dm7 Em7 F)
    (,Diatonic-I-ii-iii-biii -> C Dm7 Em7 Ebm7)
    (,Diatonic-I-ii-iii-IV-V-vi -> C Dm7 Em7 F G7 Am7)
    (,Diatonic-I-IV-iii-ii -> C F Em7 Dm7)
    (,Diatonic-ii-iii-IV -> Dm7 Em7 F)
    (,Diatonic-ii-iii-IV-V -> ,Diatonic-ii-iii-IV G7)
    (,Diatonic-ii-iii-IV-iii-ii -> Dm7 Em7 F Em7 Dm7)
    (,Dizzy-Approach -> ,Dizzy-Approach_var1)
    (,Dizzy-Approach_var1 -> Dm ,Straight-Approach)
    (,Dizzy-Approach_var2 -> Dm7b5 Db7)
    (,Dizzy-Cadence -> ,Dizzy-Cadence_main)
    (,Dizzy-Cadence_main -> ,Dizzy-Approach C)
    (,Dizzy-Cadence_var1 -> G7 ,Tritone-Cadence)
    (,Dizzy-Cadence_var2 -> ,Nowhere-Approach C)
    (,Dogleg-Approach -> ,Dogleg-Approach_short)
    (,Dogleg-Approach_short -> D7 Dm7 G7)
    (,Dogleg-Approach_long
     ->
     ,Straight-Approach
     ,Straight-Approach)
    (,Dogleg-Cadence -> ,Dogleg-Approach C)
    (,Dogleg-Cycle -> ,Dogleg-Cycle_3steps)
    (,Dogleg-Cycle_3steps
     ->
     ,Straight-Approach
     ,Straight-Approach
     ,Straight-Approach)
    (,Dogleg-Cycle_4steps
     ->
     ,Straight-Approach
     ,Dogleg-Cycle_3steps)
    (,Dogleg-Cycle_5steps
     ->
     ,Straight-Approach
     ,Dogleg-Cycle_4steps)
    (,Dogleg-Cycle_6steps
     ->
     ,Straight-Approach
     ,Dogleg-Cycle_5steps)
    (,Dominant-Cycle-2-Steps -> D7 G7)
    (,Dominant-Cycle-3-Steps -> A7 ,Dominant-Cycle-2-Steps)
    (,Dominant-Cycle-4-Steps -> E7 ,Dominant-Cycle-3-Steps)
    (,Dominant-Cycle-5-Steps -> B7 ,Dominant-Cycle-4-Steps)
    (,Dominant-Cycle-6-Steps -> F$7 ,Dominant-Cycle-5-Steps)
    (,Dominant-Cycle-7-Steps -> Db7 ,Dominant-Cycle-6-Steps)
    (,Dominant-Cycle-8-Steps -> Ab7 ,Dominant-Cycle-7-Steps)
    (,Dominant-Cycle -> ,Dominant-Cycle_Two-step)
    (,Dominant-Cycle_Two-step -> ,Dominant-Cycle-2-Steps)
    (,Dominant-Cycle_Three-step -> ,Dominant-Cycle-3-Steps)
    (,Dominant-Cycle_Four-step -> ,Dominant-Cycle-4-Steps)
    (,Dominant-Cycle_Five-step -> ,Dominant-Cycle-5-Steps)
    (,Dominant-Cycle_Six-step -> ,Dominant-Cycle-6-Steps)
    (,Dominant-Cycle_Seven-step -> ,Dominant-Cycle-7-Steps)
    (,Dominant-Cycle_Eight-step -> ,Dominant-Cycle-8-Steps)
    (,Dominant-Cycle-Cadence -> ,Dominant-Cycle ,Major-On)
    (,Dominant-Turnaround -> C7 Ab7 G7 C7)
    (,Donna-Lee-Start -> ,Donna-Lee-Start_var1)
    (,Donna-Lee-Start_var1 -> C ,Straight-Approach)
    (,Donna-Lee-Start_var2 -> C ,GenDom)
    (,Donna-Lee-Opening -> ,Donna-Lee-Opening_main)
    (,Donna-Lee-Opening_main
     ->
     ,Donna-Lee-Start
     ,Straight-Cadence)
    (,Donna-Lee-Opening_Dizzycadenceending
     ->
     ,Donna-Lee-Start
     ,Tritone-Cadence)
    (,Doo-Wop -> C Am F G)
    (,Double-Pullback -> ,An-Approach ,An-Approach ,An-Approach)
    (,Dropback -> ,Dropback_main)
    (,Dropback_main -> C A7)
    (,Dropback_iii-VI-var -> C Em7 A7)
    (,Dropback_tritone -> C Bb7 A7)
    (,Dropback_chromatic -> C B7 Bb7 A7)
    (,Dropback_TINGLe -> C ,TonicOrDom ,Nowhere-Approach)
    (,Dropback-Approach -> ,Dropback-Approach_main)
    (,Dropback-Approach_main
     ->
     ,Straight-Cadence
     ,Extended-Approach)
    (,Extended-Approach -> ,Extended-Approach_var1)
    (,Extended-Approach_var1 -> ,GenVI ,Straight-Approach)
    (,Extended-Cadence -> ,Extended-Approach C)
    (,Foggy-Cadence -> Eb7 ,Straight-Approach C)
    (,Foggy-Turnaround -> ,Foggy-Turnaround_main)
    (,Foggy-Turnaround_main -> C Eb7 ,Straight-Approach)
    (,Foggy-Turnaround_var -> C Eb7 D7 G7)
    (,Foolish-Approach -> ,Raindrop ,Straight-Approach)
    (,Four-Star-Approach -> F ,Straight-Approach ,Long-Approach)
    (,GDS-Cadence -> D/C Db/C C)
    (,Giant-Step -> ,Major-On ,Perfect-Cadence)
    (,Giant-Steps -> ,Giant-Step ,Perfect-Cadence)
    (,Giant-Step-Approach -> Ab7 ,Major-On E7)
    (,Happenstance-Cadence -> ,An-Approach C)
    (,Honeysuckle-Bridge -> ,Honeysuckle-Bridge_main)
    (,Honeysuckle-Bridge_main
     ->
     ,Straight-Cadence
     ,Straight-Approach
     ,Straight-Approach)
    (,Honeysuckle-Bridge_two-goes -> ,Two-Goes-Straight-Approach
                                  F ,Two-Goes-Straight-Approach G7)
    (,Honeysuckle-Bridge_two-goes-variant
     ->
     ,Straight-Approach
     ,Honeysuckle-Bridge)
    (,Honeysuckle-Bridge_dogleg
     ->
     ,Straight-Cadence
     ,Dogleg-Cycle)
    (,Honeysuckle-Bridge_two-goes-dogleg
     ->
     ,Two-Goes-Straight-Approach
     ,Dropback
     ,An-Approach)
    (,Honeysuckle-Bridge_pullback
     ->
     ,Pullback-to-Cadence
     ,Pullback
     ,Dominant-Cycle-2-Steps)
    (,Honeysuckle-Bridge_supertensionend
     ->
     ,Straight-Cadence
     ,Tension-Cadence)
    (,II-n-Back -> ,II-n-Back_main)
    (,II-n-Back_main -> Dm7 D$o C/E)
    (,II-n-Back_var -> Dm7 D$o Em)
    (,II-n-Bird-Approach -> ,Minor-On ,Yardbird-Approach)
    (,II-n-Bird-POT -> Dm7 ,GenDom ,POT)
    (,II-n-Bird-SPOT -> ,II-n-Bird-SPOT_main)
    (,II-n-Bird-SPOT_main -> Dm7 ,GenDom ,SPOT)
    (,II-n-Bird-SPOT_var1
     ->
     ,Minor-On
     ,Straight-Approach
     ,Rainy-Turnaround)
    (,To-IV -> ,To-IV_main) (,To-IV_main -> ,Major-On ,Cadence)
    (,IV-n-Yak -> ,MajorOrDom ,GenDom ,Major-On)
    (,To-IV-n-Yak -> ,Major-On ,SuperGenDom ,IV-n-Yak)
    (,To-IV-n-Yak-Turnaround -> ,Major-On ,SuperGenDom
                             ,MajorOrDom ,GenDom)
    (,IV-n-Yak-To -> ,IV-n-Yak C7)
    (,IV-n-Hack -> ,TonicOrDom ,Happenstance-Cadence)
    (,To-IV-n-Hack -> ,Major-On ,SuperGenDom ,IV-n-Hack)
    (,IV-n-Mack -> ,Major-On ,Minor-Plagal-Cadence)
    (,To-IV-n-Mack -> ,To-IV ,Minor-Plagal-Cadence)
    (,To-IV-n-Mack-Turnaround -> ,To-IV ,Minor-On)
    (,IV-n-Bird -> ,TonicOrDom ,GenDom ,Minor-On)
    (,To-IV-n-Bird -> ,Major-On ,SuperGenDom ,IV-n-Bird)
    (,To-IV-n-Back-SPOT -> FM7 F7 ,Surge ,SPOT)
    (,IV-n-Bird-SPOT -> ,IV-n-Bird-SPOT_main)
    (,IV-n-Bird-SPOT_main -> ,TonicOrDom ,GenDom ,SPOT)
    (,IV-n-Bird-SPOT_rainy -> ,IV-n-Bird ,Rainy-Turnaround)
    (,To-IV-n-Bird-SPOT
     ->
     ,Major-On
     ,SuperGenDom
     ,IV-n-Bird-SPOT)
    (,IV-n-Bauble -> ,Upslide ,Sad-Approach)
    (,ITCHY-Opening -> C ,An-Approach Dm ,An-Approach)
    (,La-Bomba -> C F G F)
    (,Ladybird-Turnaround -> ,Ladybird-Turnaround_main)
    (,Ladybird-Turnaround_main -> ,Major-On ,TonicOrDom
                               ,TonicOrDom ,TonicOrDom)
    (,Ladybird-Cadence -> ,Ladybird-Turnaround C)
    (,Light-&-Day-Approach -> Dm7b5 Dbm)
    (,Lonely-Approach -> ,Straight-Approach ,Straight-Approach)
    (,Lonely-Cadence -> ,Lonely-Approach C)
    (,Long-Approach -> ,Long-Approach_main)
    (,Long-Approach_main -> ,GenIII ,Extended-Approach)
    (,Long-Approach_tritone -> ,Dizzy-Approach Abm7 Db7)
    (,Long-Approach_sus -> ,Surprise-Minor-Cadence F/G)
    (,Long-Cadence -> ,Long-Cadence_main)
    (,Long-Cadence_main -> ,Long-Approach C)
    (,Minor-Chromatic-Walkdown
     ->
     ,Minor-Chromatic-Walkdown_2step)
    (,Minor-Chromatic-Walkdown_2step -> Dm7 Dbm7)
    (,Minor-Chromatic-Walkdown_3step
     ->
     ,Minor-Chromatic-Walkdown_2step
     Dbm7)
    (,Minor-Chromatic-Walkdown_4step
     ->
     ,Minor-Chromatic-Walkdown_3step
     G7)
    (,Minor-Chromatic-Walkdown-Approach
     ->
     ,Minor-Chromatic-Walkdown
     G7)
    (,Minor-Dropback -> Cm Am7b5)
    (,Minor-Plagal-Cadence -> ,Minor-On ,Major-On)
    (,Minor-Perfect-Cadence -> G7 Cm7)
    (,Moment -> ,Moment_quotes-Cadence)
    (,Moment_quotes-Cadence -> ,Moment_quotes-Approach C)
    (,Moment_quotes-Approach
     ->
     ,Straight-Approach
     ,Straight-Approach)
    (,Night-&-Day-Cadence -> AbM7 G7 C)
    (,Nobody -> ,Nobody_quotes-Cadence)
    (,Nobody_quotes-Cadence -> Eb G7 Cm)
    (,Nowhere-Approach -> ,Nowhere-Approach_main)
    (,Nowhere-Approach_main -> Ab7 ,GenDom)
    (,Nowhere-Approach_slow
     ->
     ,Straight-Approach
     ,Nowhere-Approach)
    (,Nowhere-Cadence -> ,Nowhere-Approach C)
    (,Nowhere-Minor-Cadence -> ,Nowhere-Minor-Cadence_main)
    (,Nowhere-Minor-Cadence_main -> ,Nowhere-Approach Cm)
    (,Nowhere-Minor-Cadence_var1
     ->
     ,Dominant-Cycle-2-Steps
     ,Minor-On
     Cm7/Bb)
    (,Nowhere-Turnaround -> ,Dropback ,Nowhere-Approach)
    (,Nowhere-Turnaround-Minor
     ->
     ,Minor-Dropback
     ,Nowhere-Approach)
    (,Nowhere-Turnaround+On -> ,Nowhere-Turnaround C)
    (,Nowhere-Turnaround-to-Minor-On
     ->
     ,Nowhere-Turnaround-Minor
     Cm)
    (,Passacaglia -> ,On-Off-Major-bVII ,On-Off-Major-VII)
    (,POT-Spring-sub -> C Am7 Dm7 ,Straight-Approach)
    (,POT+On -> ,POT+On_cyclic)
    (,POT+On_cyclic -> ,POT_cyclic C)
    (,POT_main -> C ,GenII ,Straight-Approach)
    (,POT_var1 -> C A7 ,Straight-Approach)
    (,POT_var2 -> ,On-Off-Major-VI ,Sad-Approach)
    (,POT_var3 -> ,Major-On ,Straight-Approach)
    (,POT_var4 -> C Am Am/C Dm7/G G7)
    (,POT_var5 -> ,Dropback ,Minor-On Gsus)
    (,POT_blues -> C7 A7 Dm7 G7) (,POT_cyclic -> C A7 D7 G7)
    (,POT_Yardbird -> C A7 Dm7 ,GenDom)
    (,POT_sadapproach -> C A7 ,Sad-Approach)
    (,POT -> C7 A7 ,Straight-Approach)
    (,Minor-POT -> ,Minor-POT_main)
    (,Minor-POT_main -> ,Minor-Dropback ,Sad-Approach)
    (,Minor-POT_var1 -> ,Minor-Dropback D7 G7)
    (,Minor-POT_var2 -> Cm Eb7 Dm7b5 G7)
    (,Minor-POT_var3 -> ,Minor-Dropback ,Straight-Approach)
    (,Minor-POT_dominant -> Cm ,Extended-Approach)
    (,Minor-POT_sad -> Cm ,Sad-Approach)
    (,Minor-POT_altereddominant -> Cm A7$11 D7 G7+)
    (,Minor-POT+On -> ,Minor-POT Cm)
    (,Pennies-Approach -> ,Pennies-Approach_main)
    (,Pennies-Approach_main -> F Bb7 ,Dropback ,An-Approach)
    (,Pennies-Approach_var1 -> F Bb7 ,GenMajorTonic ,An-Approach
                            ,An-Approach)
    (,Pennies-Approach_var2
     ->
     ,IV-n-Yak
     ,Minor-Perfect-Cadence
     ,Dominant-Cycle-2-Steps)
    (,Pennies-Ending -> ,Pennies-Ending_main)
    (,Pennies-Ending_main -> ,Pennies-Approach C)
    (,Pennies-Ending_raindrop -> Dm ,Raindrop ,Straight-Approach
                              C)
    (,Pennies-Ending_side-slipping -> ,Major-On ,Nowhere-Approach ,Stablemates-Cadence
                                   ,Straight-Approach)
    (,Pennies-Ending_dropback -> ,IV-n-Yak A7 ,Cyclic-Approach
                              C)
    (,Pennies-Ending_Somewhere
     ->
     ,IV-n-Yak
     D7
     ,Straight-Cadence)
    (,Pennies-Ending_Yardbird
     ->
     ,IV-n-Yak
     ,Minor-Perfect-Cadence
     ,Straight-Cadence)
    (,Pennies-Turnaround -> ,Pennies-Turnaround_main)
    (,Pennies-Turnaround_main -> C Dm ,Raindrop
                              ,Straight-Approach)
    (,Pennies-Turnaround_TTFA
     ->
     ,TTFA-Dropback
     ,Straight-Approach)
    (,Pennies-Turnaround-Two-Goes -> C Dm ,Raindrop
                                  ,Straight-Approach ,Straight-Approach)
    (,Perfect-Cadence -> G7 C) (,Pullback -> ,Pullback_basic)
    (,Pullback_basic -> ,An-Approach ,An-Approach)
    (,Pullback_var1 -> ,An-Approach Em7 Am7)
    (,Pullback_tritone -> ,An-Approach Bb7 A7)
    (,Rainy-Pullback -> ,Straight-Approach ,Raindrop)
    (,Pullback-Extended -> ,Pullback-Extended_main)
    (,Pullback-Extended_main -> ,Pullback ,An-Approach)
    (,Pullback-Extended_yardbird -> Dm7 Bb7 Em7 A7 Dm7 G7)
    (,Pullback-to-Cadence -> ,Pullback-Extended C)
    (,Pullback-Cadence-with-Dropback
     ->
     ,Pullback-Cadence-with-Dropback_rainy)
    (,Pullback-Cadence-with-Dropback_rainy
     ->
     ,Straight-Approach
     ,Seven-Chord-Dropback_rainy)
    (,Rainbow-Cadence -> ,Rainbow-Cadence_var1)
    (,Rainbow-Cadence_var1 -> G Bm C)
    (,Rainbow-Cadence_var2 -> G/B B+ C7$11)
    (,Rainbow-Cadence_var3 -> ,Major-On B+ ,Major-On)
    (,Rainbow-Cadence_var4 -> G B7 C)
    (,Raindrop -> ,Raindrop_main) (,Raindrop_main -> Em Ebdim)
    (,Raindrop_var1 -> C Ebdim) (,Raindrop_var2 -> A7/E Ebdim)
    (,Rainy-Turnaround -> ,Raindrop ,Straight-Approach)
    (,Rainy-Cadence -> ,Rainy-Turnaround C)
    (,Rainy-Cadence_minorchromatic
     ->
     ,Minor-Chromatic-Walkdown
     ,Straight-Cadence)
    (,Reverse-Dominant-Cycle-2-Steps -> G7 D7)
    (,Reverse-Dominant-Cycle-3-Steps
     ->
     ,Reverse-Dominant-Cycle-2-Steps
     A7)
    (,Reverse-Dominant-Cycle-4-Steps
     ->
     ,Reverse-Dominant-Cycle-3-Steps
     E7)
    (,Reverse-Dominant-Cycle-5-Steps
     ->
     ,Reverse-Dominant-Cycle-4-Steps
     B7)
    (,Reverse-Dominant-Cycle-6-Steps
     ->
     ,Reverse-Dominant-Cycle-5-Steps
     F$7)
    (,Rhythm-Bridge -> ,GenDom ,GenDom ,GenDom ,GenDom)
    (,Rhythm-Turnaround -> ,Rhythm-Turnaround_main)
    (,Rhythm-Turnaround_main -> ,Upslide Dm7 Ebo)
    (,Rhythm-Turnaround_var -> ,Upslide G7/D Ebo)
    (,Rhythm-Turnaround+On -> Dbdim ,Surge C)
    (,Sad-Approach -> Dm7b5 G7)
    (,Sad-Cadence -> ,Sad-Cadence_main)
    (,Sad-Cadence_main -> ,Sad-Approach Cm)
    (,Sad-Cadence_var -> F7$11 ,Minor-Perfect-Cadence)
    (,Sad-Cadence-with-Overrun -> ,Sad-Cadence-with-Overrun_var)
    (,Sad-Cadence-with-Overrun_var -> ,Sad-Cadence Ab7)
    (,Sad-Dropback -> Dm7b5 G7 Cm Am7b5 Dm7b5 G7)
    (,Sad-SPOT -> ,Sad-Approach ,Sad-Approach)
    (,Satin-Cadence -> ,Straight-Approach ,Straight-Approach C)
    (,Side-Slip -> C$m7b5 Cm7 F7) (,Side-Slips -> ,Side-Slips_2)
    (,Side-Slips_2 -> ,An-Approach ,An-Approach)
    (,Side-Slips_2variant -> Em Am ,An-Approach)
    (,Side-Slips_3 -> ,An-Approach ,Side-Slips_2)
    (,Side-Slips_4 -> ,Side-Slips_2 ,Side-Slips_2)
    (,Side-Slips_5 -> ,An-Approach ,Side-Slips_4)
    (,Side-Slips_6 -> ,Side-Slips_2 ,Side-Slips_4)
    (,Sharp-Fourpenny-Approach -> F$m7b5 ,Yardbird-Cadence
                               ,Straight-Approach ,Straight-Approach)
    (,Sharp-Fourpenny-Ending -> ,Sharp-Fourpenny-Ending_main)
    (,Sharp-Fourpenny-Ending_main
     ->
     ,Sharp-Fourpenny-Approach
     C)
    (,Sharp-Fourpenny-Ending_var
     ->
     ,Side-Slip
     ,Straight-Approach
     ,Sus-Cadence)
    (,Sixpenny-Approach -> Am ,Yardbird-Cadence
                        ,Straight-Approach ,Straight-Approach)
    (,Sixpenny-Ending -> ,Sixpenny-Approach C)
    (,Spring-Approach -> ,Spring-Approach_simple)
    (,Spring-Approach_simple -> ,GenII ,Straight-Approach)
    (,Spring-Approach_extended -> ,GenII E7 ,Straight-Approach)
    (,Spring-Cadence -> ,Spring-Cadence_main)
    (,Spring-Cadence_main -> ,Spring-Approach C)
    (,Somewhere/Nowhere-Approach -> D7 Ab7)
    (,SPOT -> ,SPOT_main)
    (,SPOT_main -> ,An-Approach ,Straight-Approach)
    (,SPOT_minorIV -> Em7 ,Extended-Approach)
    (,SPOT_var1
     ->
     Em7
     ,Minor-Perfect-Cadence
     ,Straight-Approach)
    (,SPOT_var2 -> ,Straight-Approach ,Straight-Approach G7/F)
    (,SPOT_var3 -> ,Tritone-Approach ,Straight-Approach)
    (,SPOT_sideslip -> Em7 Ebm7 Ab7 Dm7 G7)
    (,Multi-Sub-POT -> C Bbm7 Eb7 Am7 D7 Abm7 Db7)
    (,Stablemates-Approach -> ,Straight-Approach ,An-Approach)
    (,Stablemates-Cadence -> ,Stablemates-Approach C)
    (,Starlight-Approach -> ,Starlight-Approach_main)
    (,Starlight-Approach_main
     ->
     ,An-Approach
     ,An-Approach
     ,An-Approach)
    (,Starlight-Approach_Dizzy -> F$m7b5 B7 ,Tritone-Approach
                               ,Tritone-Approach)
    (,Starlight-Approach_Dizzyvar2 -> ,Dizzy-Approach ,SPOT)
    (,Starlight-Approach_Night-&-Day
     ->
     ,Light-&-Day-Approach
     ,SPOT)
    (,Starlight-Approach_rainy -> F$m7b5 Fm ,Rainy-Turnaround)
    (,Starlight-Approach_tritonestart
     ->
     ,Tritone-Approach
     ,SPOT)
    (,Starlight-Cadence -> ,Starlight-Cadence_main)
    (,Starlight-Cadence_main -> ,Starlight-Approach C)
    (,Starlight-Cadence_airegin
     ->
     ,Sad-Approach
     ,Sad-Cadence
     ,Perfect-Cadence)
    (,Starlight-Cadence_tension -> ,Starlight-Approach C7)
    (,Starlight-Cadence_Night-&-Day
     ->
     ,Starlight-Approach_Night-&-Day
     C)
    (,Starlight-Dropback -> ,Sad-Approach ,Straight-Approach)
    (,Starlight-Opening -> ,Sad-Approach ,Straight-Approach)
    (,Straight-Approach -> ,Straight-Approach_main)
    (,Straight-Approach_main -> Dm7 G7)
    (,Straight-Approach_var -> Gm7 C7 C7/Bb)
    (,Straight-Cadence -> ,Straight-Cadence_main)
    (,Straight-Cadence_main -> ,Straight-Approach C)
    (,Straight-Cadence-with-Dropback
     ->
     ,Straight-Approach
     C
     Am7)
    (,Straight-Cadence-+-Dominant-Overrun
     ->
     ,Straight-Cadence
     F7)
    (,Surge -> ,Surge_major) (,Surge_major -> ,Major-On C$o)
    (,Surge_minor -> ,Minor-On C$o) (,Surge_dominant -> C7 C$o)
    (,Surge_majorvar -> ,Surge_major G/D)
    (,Surge_minorvar -> ,Surge_minor G/D)
    (,Surge_dominantvar -> ,Surge_dominant G/D)
    (,Supertension-Ending -> ,Straight-Approach C7$4)
    (,Surprise-Minor-Cadence -> ,Straight-Approach Cm)
    (,Surprise-Major-Cadence -> ,Sad-Approach C)
    (,Sus-Approach -> ,Major-On D7 ,Major-On)
    (,Sus-Approach_var1 -> ,Minor-On Gsus)
    (,Sus-Cadence -> Gm7 Gm7/C ,Major-On)
    (,Sus-Cadence_main -> G7sus C)
    (,Sus-Cadence_var1 -> Dm7 G7sus C)
    (,Sus-Cadence_var2 -> G7sus G7 C)
    (,Tension-Cadence -> Cm7 F7 Bb7)
    (,Tension-Pullback
     ->
     ,Straight-Approach
     ,Dominant-Cycle-2-Steps)
    (,Tension-SPOT
     ->
     ,Dominant-Cycle-2-Steps
     ,Straight-Approach)
    (,To-II-n-Back -> C ,Minor-On Ebm7b5 ,Major-On)
    (,To-Somewhere -> C D7) (,Cadence -> ,Cadence_perfect)
    (,Cadence_perfect -> ,Perfect-Cadence)
    (,Cadence_straight -> ,Straight-Cadence)
    (,Cadence_sad -> ,Sad-Cadence)
    (,Cadence_tritone -> ,Tritone-Cadence)
    (,Cadence_surge -> ,TonicOrDom F$o C)
    (,To-IV-n-Bird-POT -> C ,GenDom FM7 Bb7 ,POT)
    (,TTFA-Dropback -> ,TTFA-Dropback_main)
    (,TTFA-Dropback_main -> C ,Straight-Approach)
    (,TTFA-Dropback_dropback -> C/G ,Minor-Perfect-Cadence A7)
    (,TTFA-Dropback_IV-variant
     ->
     C
     ,TonicOrDom
     ,Liberal-Approach)
    (,TTFA-Dropback_ii-variant -> C Dm7 ,Straight-Approach)
    (,TTFA-Dropback_IInBackvariant -> ,Major-On ,II-n-Back Am7)
    (,TTFA-Dropback_rainy -> C F7 Em7 Ebo)
    (,TTFA-Dropback_minorchromaticdescent
     ->
     C
     F7
     ,Minor-Chromatic-Walkdown)
    (,Twopenny-Approach -> Dm ,Yardbird-Cadence
                        ,Straight-Approach ,Straight-Approach)
    (,Twopenny-Ending -> ,Twopenny-Ending_main)
    (,Twopenny-Ending_main -> ,Twopenny-Approach C)
    (,Twopenny-Ending_Nowheredropback -> ,Yardbird-Sub-Cadence
                                      Abm7 Db7 Gm7 C7 FM7)
    (,Twopenny-Ending_Starlightback -> ,Yardbird-Sub-Cadence
                                    Bm7b5 Bb7 Am7 D7 Gm7 C7 FM7)
    (,Twopenny-Ending_var1 -> Am7 ,GenDom ,Straight-Approach
                           ,Stablemates-Cadence)
    (,Twopenny-Ending_var2 -> Dm7 B7 Em7 A7 ,An-Approach C)
    (,Twopenny-Ending_var3 -> ,II-n-Bird-SPOT EbM7)
    (,Upslide -> C Dbdim) (,Tritone-Approach -> Dm7 Db7)
    (,Tritone-Cadence -> ,Tritone-Cadence_main)
    (,Tritone-Cadence_main -> Dm7 Db7 C)
    (,Tritone-Cadence_short -> Db7 C)
    (,Tritone/Straight-Approach
     ->
     ,Tritone-Approach
     ,Straight-Approach
     C7/Bb)
    (,Two-Goes-Approach -> ,An-Approach ,An-Approach)
    (,Two-Goes-Nowhere-Approach
     ->
     ,Nowhere-Approach
     ,Nowhere-Approach)
    (,Two-Goes-Nowhere-Cadence -> ,Two-Goes-Nowhere-Approach C)
    (,Two-Goes-Pullback -> ,Pullback_basic ,Pullback_basic)
    (,Two-Goes-Pullback-Extended
     ->
     ,Two-Goes-Pullback
     ,Straight-Approach)
    (,Two-Goes-Pullback+Cadence
     ->
     ,Two-Goes-Pullback-Extended
     C)
    (,Two-Goes-Rainy-Turnaround
     ->
     ,Rainy-Turnaround
     ,Rainy-Turnaround)
    (,Two-Goes-Sad-Approach -> ,Sad-Approach ,Sad-Approach)
    (,Two-Goes-Sad-Cadence -> ,Two-Goes-Sad-Approach Cm)
    (,Two-Goes-Starlight-Approach
     ->
     ,Straight-Approach
     ,Starlight-Approach)
    (,Two-Goes-Straight-Approach
     ->
     ,Straight-Approach
     ,Straight-Approach)
    (,Two-Goes-Straight-Cadence
     ->
     ,Two-Goes-Straight-Approach
     C)
    (,Two-Goes-Straight-Cadence-with-Overrun
     ->
     ,Two-Goes-Straight-Cadence-with-Overrun_var1)
    (,Two-Goes-Straight-Cadence-with-Overrun_var1
     ->
     ,Two-Goes-Straight-Cadence
     F)
    (,Two-Goes-Straight-Cadence-with-Overrun_var2
     ->
     ,Two-Goes-Straight-Cadence
     F7)
    (,Three-Goes-Pullback -> ,Two-Goes-Pullback ,Pullback)
    (,Three-Goes-Pullback-Extended
     ->
     ,Three-Goes-Pullback
     ,Straight-Approach)
    (,Three-Goes-Pullback+Cadence
     ->
     ,Three-Goes-Pullback-Extended
     C)
    (,Three-Goes-Sad-Approach
     ->
     ,Two-Goes-Sad-Approach
     ,Sad-Approach)
    (,Three-Goes-Sad-Cadence -> ,Three-Goes-Sad-Approach Cm)
    (,Three-Goes-Straight-Approach
     ->
     ,Two-Goes-Straight-Approach
     ,Straight-Approach)
    (,Three-Goes-Straight-Cadence
     ->
     ,Three-Goes-Straight-Approach
     C)
    (,Three-Goes-Nowhere-Approach
     ->
     ,Two-Goes-Nowhere-Approach
     ,Nowhere-Approach)
    (,Three-Goes-Nowhere-Cadence
     ->
     ,Three-Goes-Nowhere-Approach
     C)
    (,Four-Goes-Straight-Approach
     ->
     ,Two-Goes-Straight-Approach
     ,Two-Goes-Straight-Approach)
    (,Four-Goes-Straight-Cadence
     ->
     ,Four-Goes-Straight-Approach
     C)
    (,Four-Goes-Sad-Approach
     ->
     ,Two-Goes-Sad-Approach
     ,Two-Goes-Sad-Approach)
    (,Four-Goes-Sad-Cadence -> ,Four-Goes-Sad-Approach Cm)
    (,Two-Goes-Tritone-Approach
     ->
     ,Tritone-Approach
     ,Tritone-Approach)
    (,Two-Goes-Tritone-Cadence -> ,Two-Goes-Tritone-Approach C)
    (,Three-Goes-Tritone-Approach
     ->
     ,Two-Goes-Tritone-Approach
     ,Tritone-Approach)
    (,Three-Goes-Tritone-Cadence
     ->
     ,Three-Goes-Tritone-Approach
     C)
    (,Four-Goes-Tritone-Approach
     ->
     ,Three-Goes-Tritone-Approach
     ,Tritone-Approach)
    (,Four-Goes-Tritone-Cadence
     ->
     ,Four-Goes-Tritone-Approach
     C)
    (,Two-Goes-Dominant-Approach
     ->
     ,Dominant-Cycle-2-Steps
     ,Dominant-Cycle-2-Steps)
    (,Two-Goes-Tritone-Straight-Approach
     ->
     ,Tritone-Approach
     ,Straight-Approach)
    (,Two-Goes-Tritone-Straight-Cadence
     ->
     ,Two-Goes-Tritone-Straight-Approach
     C)
    (,Whoopee-Turnaround -> ,Upslide ,Straight-Approach)
    (,Whoopee-Cadence -> Dbdim ,Straight-Cadence)
    (,Wonderful-Opening -> ,Surge ,Straight-Cadence)
    (,Yardbird-Approach -> Fm7 Bb7)
    (,Yardbird-Cadence -> ,Yardbird-Cadence_main)
    (,Yardbird-Cadence_main -> ,Yardbird-Approach C)
    (,Yardbird-Cadence_var -> Bb7 C)
    (,Yardbird-Sub-Cadence -> Dm7 ,GenDom C)
    (,Yardbird-Sub-Turnaround -> C ,Minor-On Bb7)
    (,Major-On -> C) (,Minor-On -> ,Minor-On_main)
    (,Minor-On_main -> Cm) (,Minor-On_var1 -> CmM7)
    (,Minor-On_var2 -> Cm6) (,Minor-On_var3 -> Cm7)
    (,On-Off-Major-V -> C G7)
    (,On-Off-Major-bII -> ,Major-On ,Off)
    (,On-Off-Major-Somewhere -> ,Major-On ,Off)
    (,On-Off-Major-bIII -> ,Major-On ,Off)
    (,On-Off-Major-III -> ,Major-On ,Off)
    (,On-Off-Major-IV -> ,Major-On ,Off)
    (,On-Off-Major-$IV -> ,Major-On ,Off)
    (,On-Off-Major-Nowhere -> ,Major-On ,Off)
    (,On-Off-Major-VI -> ,Major-On ,Off)
    (,On-Off-Major-bVII -> ,Major-On ,Off)
    (,On-Off-Major-VII -> ,Major-On ,Off)
    (,On-Off-Minor-bII -> ,Minor-On ,Off)
    (,On-Off-Minor-Somewhere -> ,Minor-On ,Off)
    (,On-Off-Minor-bIII -> ,Minor-On ,Off)
    (,On-Off-Minor-III -> ,Minor-On ,Off)
    (,On-Off-Minor-IV -> ,Minor-On ,Off)
    (,On-Off-Minor-$IV -> ,Minor-On ,Off)
    (,On-Off-Minor-V -> ,GenMinor G7)
    (,On-Off-Minor-Nowhere -> ,Minor-On ,Off)
    (,On-Off-Minor-VI -> ,Minor-On ,Off)
    (,On-Off-Minor-bVII -> ,Minor-On ,Off)
    (,On-Off-Minor-VII -> ,Minor-On ,Off)
    (,Off-On-Major-bII -> ,Off ,Major-On)
    (,Off-On-Major-Somewhere -> ,Off ,Major-On)
    (,Off-On-Major-bIII -> ,Off ,Major-On)
    (,Off-On-Major-III -> ,Off ,Major-On)
    (,Off-On-Major-IV -> ,Off ,Major-On)
    (,Off-On-Major-$IV -> ,Off ,Major-On)
    (,Off-On-Major-Nowhere -> ,Off ,Major-On)
    (,Off-On-Major-VI -> ,Off ,Major-On)
    (,Off-On-Major-bVII -> ,Off ,Major-On)
    (,Off-On-Major-VII -> ,Off ,Major-On)
    (,Off-On-Minor-bII -> ,Off ,Minor-On)
    (,Off-On-Minor-Somewhere -> ,Off ,Minor-On)
    (,Off-On-Minor-bIII -> ,Off ,Minor-On)
    (,Off-On-Minor-III -> ,Off ,Minor-On)
    (,Off-On-Minor-IV -> ,Off ,Minor-On)
    (,Off-On-Minor-$IV -> ,Off ,Minor-On)
    (,Off-On-Minor-Nowhere -> ,Off ,Minor-On)
    (,Off-On-Minor-VI -> ,Off ,Minor-On)
    (,Off-On-Minor-bVII -> ,Off ,Minor-On)
    (,Off-On-Minor-VII -> ,Off ,Minor-On)
    (,On-Off-Twice-To-V -> ,Major-On G7 ,Major-On G7)
    (,On-Off-On-To-IV-Major -> ,Major-On ,Amen-Cadence)
    (,On-Off-On-+Dropback-IV-Major
     ->
     ,On-Off-On-To-IV-Major
     ,SuperGenDom)
    (,On-Off-On-To-IV-Minor -> ,Minor-On Fm ,Minor-On)
    (,On-Off-Twice-On-To-IV
     ->
     ,On-Off-On-To-IV-Major
     ,Amen-Cadence)
    (,On-Off-Thrice-To-IV
     ->
     ,On-Off-Major-IV
     ,On-Off-Major-IV
     ,On-Off-Major-IV)
    (,On-Off-Thrice-On-To-IV
     ->
     ,On-Off-Twice-On-To-IV
     ,Amen-Cadence)
    (,On-Off-Four-Times-On-To-IV
     ->
     ,On-Off-Thrice-On-To-IV
     ,Amen-Cadence)
    (,On-Off-On-To-V -> ,On-Off-On-To-V_main)
    (,On-Off-On-To-V_main -> ,Major-On ,Perfect-Cadence)
    (,On-Off-On-To-V_minor -> ,Minor-On ,DomOrMinor ,Minor-On)
    (,On-Off-On-To-V_sus -> ,Major-On Gsus ,Major-On)
    (,On-Off-Twice-On-To-V -> ,On-Off-On-To-V ,Perfect-Cadence)
    (,On-Off-Thrice-On-To-V
     ->
     ,On-Off-Twice-On-To-V
     ,Perfect-Cadence)
    (,On-Off-Four-Times-On-To-V
     ->
     ,On-Off-Thrice-On-To-V
     ,Perfect-Cadence)
    (,On-Off-On-To-II -> ,Major-On Dm7 ,Major-On)
    (,On-Off-Twice-To-II -> ,Major-On Dm7 ,Major-On Dm7)
    (,On-Off-Twice-On-To-II -> ,On-Off-On-To-II Dm7 ,Major-On)
    (,On-Off-Thrice-On-To-II
     ->
     ,On-Off-Twice-On-To-II
     Dm7
     ,Major-On)
    (,On-Off-Four-Times-On-To-II
     ->
     ,On-Off-Thrice-On-To-II
     Dm7
     ,Major-On)
    (,On-Off-To-IV-Twice-Major-then-Minor
     ->
     ,On-Off-Major-IV
     ,On-Off-Minor-IV)
    (,On-Off-Major-Minor-V -> ,On-Off-Major-V ,On-Off-Minor-V)
    (,On-Off-Twice-Minor-V -> ,On-Off-Minor-V ,On-Off-Minor-V)
    (,On-Off-Thrice-Minor-V
     ->
     ,On-Off-Twice-Minor-V
     ,On-Off-Minor-V)
    (,On-Off-On-Minor-V -> ,On-Off-Minor-V ,GenMinor)
    (,On-Off-On-Twice-Minor-V
     ->
     ,On-Off-Twice-Minor-V
     ,GenMinor)
    (,On-Off-On-To-bVII -> ,Major-On Bb7 ,Major-On)
    (,On-Off-Twice-On-To-bVII
     ->
     ,On-Off-On-To-bVII
     Bb7
     ,Major-On)
    (,On-Off-Thrice-On-To-bVII
     ->
     ,On-Off-Twice-On-To-bVII
     Bb7
     ,Major-On)
    (,On-Off-Four-Times-On-To-bVII
     ->
     ,On-Off-Thrice-On-To-bVII
     Bb7
     ,Major-On)
    (,Diatonic-I-ii-iii -> ,Major-On ,Minor-On ,Minor-On)
    (,Diatonic-I-ii-iii-vi -> GM7 Am7 Bm7 Em7)
    (,On-Off-to-bVII -> ,Major-On ,MajorOrDom)
    (,On-Off-Twice-to-bVII -> ,On-Off-to-bVII ,On-Off-to-bVII)
    (,To-IV-n-Back -> ,Major-On ,SuperGenDom ,IV-n-Back)
    (,To-IV-n-Back_blues -> F7_ ,IV-n-Back_blues)
    (,IV-n-Back -> ,MajorOrDom F$o C)
    (,IV-n-Back_blues -> Bb7 Bo7 F7_)
    (,Diatonic-Walk-Down -> Em7 Dm7 C)
    (,Minor-ii-to-Minor-v
     ->
     ,On-Off-Minor-Somewhere
     ,Off-On-Minor-VII
     ,Minor-On)
    (,To-IV-+-Bird-Approach -> Bb Bb7 Eb Ab7)
    (,Daahoud-Cadence -> Abm7 Gb7 F7 EM7 EbM7)
    (,Rainy-Pullback-Extended
     ->
     ,Rainy-Pullback
     ,Straight-Approach)
    (,Rainbow-Overrun -> C E7 F Bb7)
    (,Rainbow-Overrun_sub1
     ->
     ,Minor-On
     ,Off-On-Major-VII
     ,Minor-On)
    (,Major-Off-On -> ,Major-Off-On_ii-I)
    (,Major-Off-On_ii-I -> Fm7b5 ,Major-On)
    (,Major-Off-On_iv-I -> Abm ,Major-On)
    (,Major-Off-On-Off -> ,Major-Off-On Fm7b5)
    (,Diatonic-ii-I -> ,Minor-On ,Major-On)
    (,Approach-from-iii -> ,Sad-Cadence ,Nowhere-Approach)
    (,Four-Star-Ending -> ,Four-Star-Approach C)
    (,Four-Star-Ending_var1
     ->
     ,Major-On
     ,Approach-from-iii
     ,Body-&-Soul-Cadence)
    (,Major-On-+-Dropback -> C6 Em7b5/Bb A7)
    (,Descending-Minor-CESH-Approach -> Gm7 Gm/F$ Gm7/F C7)
    (,On-+-Dropback -> ,Major-On ,Gen7)
    (,Tritone-Sub-Turnaround
     ->
     ,On-+-Dropback
     ,Tritone-Approach)
    (,Somewhere-Turnaround
     ->
     ,Donna-Lee-Start
     ,Straight-Approach)
    (,Giant-Step-Turnaround -> ,Giant-Step-Turnaround_var1)
    (,Giant-Step-Turnaround_var1 -> ,Giant-Step Bb7sus4 Bb7)
    (,Giant-Step-Turnaround_var2 -> ,Giant-Step Bb7sus4 Bb7)
    (,Seven-sus4-to-3 -> Bb7sus4 Bb7)))

(define S-rules
  `((S -> ,GenMinor_var1)
    (S -> ,GenMinor_var2) (S -> ,GenDom_var1)
    (S -> ,GenDom_var2) (S -> ,GenII_var1) (S -> ,GenII_var2)
    (S -> ,LiberalII_var1) (S -> ,LiberalII_var2)
    (S -> ,GenIII_var1) (S -> ,GenIII_var2)
    (S -> ,GenIII_var3) (S -> ,GenVI_var1) (S -> ,GenVI_var2)
    (S -> ,GenVI_var3) (S -> ,Gen7_var1) (S -> ,Gen7_var2)
    (S -> ,An-Approach) (S -> ,Liberal-Approach_var1)
    (S -> ,Liberal-Approach_var2) (S -> SuperGenDom_var1)
    (S -> SuperGenDom_var2) (S -> SuperGenDom_var3)
    (S -> ,GenMajorTonic_main) (S -> ,GenMajorTonic_iii)
    (S -> ,Off_dominant) (S -> ,Off_major) (S -> ,Off_minor)
    (S -> ,Off_minor7) (S -> ,Off_minor-major)
    (S -> ,Off_diminished) (S -> ,TonicOrDom_major)
    (S -> ,TonicOrDom_minor) (S -> ,TonicOrDom_dom)
    (S -> ,MajorOrDom_major) (S -> ,MajorOrDom_dom)
    (S -> ,DomOrMinor_minor) (S -> ,DomOrMinor_minor7)
    (S -> ,DomOrMinor_dom) (S -> ,Ascending-Minor-CESH_3step)
    (S -> ,Ascending-Minor-CESH_4step)
    (S -> ,Ascending-Minor-CESH-Approach)
    (S -> ,Descending-Minor-CESH_3step)
    (S -> ,Descending-Minor-CESH_4step)
    (S -> ,Ascending-Major-CESH_3step)
    (S -> ,Ascending-Major-CESH_4step)
    (S -> ,Ascending-&-Descending-Minor-CESH_4step)
    (S -> ,Ascending-&-Descending-Minor-CESH_5step)
    (S -> ,Ascending-&-Descending-Major-CESH)
    (S -> Seven-Chord-Dropback_main)
    (S -> Seven-Chord-Dropback_minoron)
    (S -> Seven-Chord-Dropback_rainy) (S -> ,Amen-Cadence)
    (S -> ,Autumn-Leaves-Opening_main)
    (S -> ,Autumn-Leaves-Opening_withoverrun)
    (S -> ,Autumnal-Approach_var1)
    (S -> ,Autumnal-Approach_var2)
    (S -> ,Autumnal-Cadence_main)
    (S -> ,Autumnal-Cadence_var1)
    (S -> ,Autumnal-Cadence_var2) (S -> ,Bemsha-Turnaround-1)
    (S -> ,Bemsha-Turnaround-2) (S -> ,Bemsha-Turnaround-3)
    (S -> ,Body-&-Soul-Approach_main)
    (S -> ,Body-&-Soul-Approach_var1)
    (S -> ,Body-&-Soul-Approach_var2)
    (S -> ,Body-&-Soul-Cadence_major)
    (S -> ,Body-&-Soul-Cadence_minor)
    (S -> ,Chromatic-Dropback_main)
    (S -> ,Chromatic-Major-Walkup)
    (S -> ,Chromatic-Minor-Ascending_3steps)
    (S -> ,Chromatic-Minor-Ascending_4steps)
    (S -> ,Chromatic-Minor-Descending_3steps)
    (S -> ,Chromatic-Minor-Descending_4steps)
    (S -> ,Chromatically-Descending-Dominants_3steps)
    (S -> ,Chromatically-Descending-Dominants_4steps)
    (S -> ,Chromatically-Descending-Dominants_5steps)
    (S -> ,Chromatically-Descending-Dominants_6steps)
    (S -> ,Coltrane-Cadence) (S -> ,Cyclic-Approach_2)
    (S -> ,Cyclic-Approach_3)
    (S -> ,Diatonic-Walkup_3stepsfromii)
    (S -> ,Diatonic-Walkup_4stepsfromii)
    (S -> ,Diatonic-I-ii-iii-ii) (S -> ,Diatonic-I-ii-iii-IV)
    (S -> ,Diatonic-I-ii-iii-biii)
    (S -> ,Diatonic-I-ii-iii-IV-V-vi)
    (S -> ,Diatonic-I-IV-iii-ii) (S -> ,Diatonic-ii-iii-IV)
    (S -> ,Diatonic-ii-iii-IV-V)
    (S -> ,Diatonic-ii-iii-IV-iii-ii)
    (S -> ,Dizzy-Approach_var1) (S -> ,Dizzy-Approach_var2)
    (S -> ,Dizzy-Cadence_main) (S -> ,Dizzy-Cadence_var1)
    (S -> ,Dizzy-Cadence_var2) (S -> ,Dogleg-Approach_short)
    (S -> ,Dogleg-Approach_long) (S -> ,Dogleg-Cadence)
    (S -> ,Dogleg-Cycle_3steps) (S -> ,Dogleg-Cycle_4steps)
    (S -> ,Dogleg-Cycle_5steps) (S -> ,Dogleg-Cycle_6steps)
    (S -> ,Dominant-Cycle-2-Steps)
    (S -> ,Dominant-Cycle-3-Steps)
    (S -> ,Dominant-Cycle-4-Steps)
    (S -> ,Dominant-Cycle-5-Steps)
    (S -> ,Dominant-Cycle-6-Steps)
    (S -> ,Dominant-Cycle-7-Steps)
    (S -> ,Dominant-Cycle-8-Steps)
    (S -> ,Dominant-Cycle_Two-step)
    (S -> ,Dominant-Cycle_Three-step)
    (S -> ,Dominant-Cycle_Four-step)
    (S -> ,Dominant-Cycle_Five-step)
    (S -> ,Dominant-Cycle_Six-step)
    (S -> ,Dominant-Cycle_Seven-step)
    (S -> ,Dominant-Cycle_Eight-step)
    (S -> ,Dominant-Cycle-Cadence) (S -> ,Dominant-Turnaround)
    (S -> ,Donna-Lee-Start_var1) (S -> ,Donna-Lee-Start_var2)
    (S -> ,Donna-Lee-Opening_main)
    (S -> ,Donna-Lee-Opening_Dizzycadenceending)
    (S -> ,Doo-Wop) (S -> ,Double-Pullback)
    (S -> ,Dropback_main) (S -> ,Dropback_iii-VI-var)
    (S -> ,Dropback_tritone) (S -> ,Dropback_chromatic)
    (S -> ,Dropback_TINGLe) (S -> ,Dropback-Approach_main)
    (S -> ,Extended-Approach_var1) (S -> ,Extended-Cadence)
    (S -> ,Foggy-Cadence) (S -> ,Foggy-Turnaround_main)
    (S -> ,Foggy-Turnaround_var) (S -> ,Foolish-Approach)
    (S -> ,Four-Star-Approach) (S -> ,GDS-Cadence)
    (S -> ,Giant-Step) (S -> ,Giant-Steps)
    (S -> ,Giant-Step-Approach) (S -> ,Happenstance-Cadence)
    (S -> ,Honeysuckle-Bridge_main)
    (S -> ,Honeysuckle-Bridge_two-goes)
    (S -> ,Honeysuckle-Bridge_two-goes-variant)
    (S -> ,Honeysuckle-Bridge_dogleg)
    (S -> ,Honeysuckle-Bridge_two-goes-dogleg)
    (S -> ,Honeysuckle-Bridge_pullback)
    (S -> ,Honeysuckle-Bridge_supertensionend)
    (S -> ,II-n-Back_main) (S -> ,II-n-Back_var)
    (S -> ,II-n-Bird-Approach) (S -> ,II-n-Bird-POT)
    (S -> ,II-n-Bird-SPOT_main) (S -> ,II-n-Bird-SPOT_var1)
    (S -> ,To-IV_main) (S -> ,IV-n-Yak) (S -> ,To-IV-n-Yak)
    (S -> ,To-IV-n-Yak-Turnaround) (S -> ,IV-n-Yak-To)
    (S -> ,IV-n-Hack) (S -> ,To-IV-n-Hack) (S -> ,IV-n-Mack)
    (S -> ,To-IV-n-Mack) (S -> ,To-IV-n-Mack-Turnaround)
    (S -> ,IV-n-Bird) (S -> ,To-IV-n-Bird)
    (S -> ,To-IV-n-Back-SPOT) (S -> ,IV-n-Bird-SPOT_main)
    (S -> ,IV-n-Bird-SPOT_rainy) (S -> ,To-IV-n-Bird-SPOT)
    (S -> ,IV-n-Bauble) (S -> ,ITCHY-Opening)
    (S -> ,La-Bomba) (S -> ,Ladybird-Turnaround_main)
    (S -> ,Ladybird-Cadence) (S -> ,Light-&-Day-Approach)
    (S -> ,Lonely-Approach) (S -> ,Lonely-Cadence)
    (S -> ,Long-Approach_main) (S -> ,Long-Approach_tritone)
    (S -> ,Long-Approach_sus) (S -> ,Long-Cadence_main)
    (S -> ,Minor-Chromatic-Walkdown_2step)
    (S -> ,Minor-Chromatic-Walkdown_3step)
    (S -> ,Minor-Chromatic-Walkdown_4step)
    (S -> ,Minor-Chromatic-Walkdown-Approach)
    (S -> ,Minor-Dropback)
    (S -> ,Minor-Plagal-Cadence)
    (S -> ,Minor-Perfect-Cadence)
    (S -> ,Moment_quotes-Cadence)
    (S -> ,Moment_quotes-Approach)
    (S -> ,Night-&-Day-Cadence)
    (S -> ,Nobody_quotes-Cadence)
    (S -> ,Nowhere-Approach_main)
    (S -> ,Nowhere-Approach_slow)
    (S -> ,Nowhere-Cadence)
    (S -> ,Nowhere-Minor-Cadence_main)
    (S -> ,Nowhere-Minor-Cadence_var1)
    (S -> ,Nowhere-Turnaround)
    (S -> ,Nowhere-Turnaround-Minor)
    (S -> ,Nowhere-Turnaround+On)
    (S -> ,Nowhere-Turnaround-to-Minor-On)
    (S -> ,Passacaglia)
    (S -> ,POT-Spring-sub)
    (S -> ,POT+On_cyclic)
    (S -> ,POT_main)
    (S -> ,POT_var1)
    (S -> ,POT_var2)
    (S -> ,POT_var3)
    (S -> ,POT_var4)
    (S -> ,POT_var5)
    (S -> ,POT_blues)
    (S -> ,POT_cyclic)
    (S -> ,POT_Yardbird)
    (S -> ,POT_sadapproach)
    (S -> ,POT)
    (S -> ,Minor-POT_main)
    (S -> ,Minor-POT_var1)
    (S -> ,Minor-POT_var2)
    (S -> ,Minor-POT_var3)
    (S -> ,Minor-POT_dominant)
    (S -> ,Minor-POT_sad)
    (S -> ,Minor-POT_altereddominant)
    (S -> ,Minor-POT+On)
    (S -> ,Pennies-Approach_main)
    (S -> ,Pennies-Approach_var1)
    (S -> ,Pennies-Approach_var2)
    (S -> ,Pennies-Ending_main)
    (S -> ,Pennies-Ending_raindrop)
    (S -> ,Pennies-Ending_side-slipping)
    (S -> ,Pennies-Ending_dropback)
    (S -> ,Pennies-Ending_Somewhere)
    (S -> ,Pennies-Ending_Yardbird)
    (S -> ,Pennies-Turnaround_main)
    (S -> ,Pennies-Turnaround_TTFA)
    (S -> ,Pennies-Turnaround-Two-Goes)
    (S -> ,Perfect-Cadence)
    (S -> ,Pullback_basic)
    (S -> ,Pullback_var1)
    (S -> ,Pullback_tritone)
    (S -> ,Rainy-Pullback)
    (S -> ,Pullback-Extended_main)
    (S -> ,Pullback-Extended_yardbird)
    (S -> ,Pullback-to-Cadence)
    (S -> ,Pullback-Cadence-with-Dropback_rainy)
    (S -> ,Rainbow-Cadence_var1)
    (S -> ,Rainbow-Cadence_var2)
    (S -> ,Rainbow-Cadence_var3)
    (S -> ,Rainbow-Cadence_var4)
    (S -> ,Raindrop_main)
    (S -> ,Raindrop_var1)
    (S -> ,Raindrop_var2)
    (S -> ,Rainy-Turnaround)
    (S -> ,Rainy-Cadence)
    (S -> ,Rainy-Cadence_minorchromatic)
    (S -> ,Reverse-Dominant-Cycle-2-Steps)
    (S -> ,Reverse-Dominant-Cycle-3-Steps)
    (S -> ,Reverse-Dominant-Cycle-4-Steps)
    (S -> ,Reverse-Dominant-Cycle-5-Steps)
    (S -> ,Reverse-Dominant-Cycle-6-Steps)
    (S -> ,Rhythm-Bridge)
    (S -> ,Rhythm-Turnaround_main)
    (S -> ,Rhythm-Turnaround_var)
    (S -> ,Rhythm-Turnaround+On)
    (S -> Sad-Approach)
    (S -> Sad-Cadence_main)
    (S -> Sad-Cadence_var)
    (S -> Sad-Cadence-with-Overrun_var)
    (S -> Sad-Dropback)
    (S -> Sad-SPOT)
    (S -> Satin-Cadence)
    (S -> Side-Slip)
    (S -> Side-Slips_2)
    (S -> Side-Slips_2variant)
    (S -> Side-Slips_3)
    (S -> Side-Slips_4)
    (S -> Side-Slips_5)
    (S -> Side-Slips_6)
    (S -> Sharp-Fourpenny-Approach)
    (S -> Sharp-Fourpenny-Ending_main)
    (S -> Sharp-Fourpenny-Ending_var)
    (S -> Sixpenny-Approach)
    (S -> Sixpenny-Ending)
    (S -> Spring-Approach_simple)
    (S -> Spring-Approach_extended)
    (S -> Spring-Cadence_main)
    (S -> Somewhere/Nowhere-Approach)
    (S -> SPOT_main)
    (S -> SPOT_minorIV)
    (S -> SPOT_var1)
    (S -> SPOT_var2)
    (S -> SPOT_var3)
    (S -> SPOT_sideslip)
    (S -> ,Multi-Sub-POT)
    (S -> Stablemates-Approach)
    (S -> Stablemates-Cadence)
    (S -> Starlight-Approach_main)
    (S -> Starlight-Approach_Dizzy)
    (S -> Starlight-Approach_Dizzyvar2)
    (S -> Starlight-Approach_Night-&-Day)
    (S -> Starlight-Approach_rainy)
    (S -> Starlight-Approach_tritonestart)
    (S -> Starlight-Cadence_main)
    (S -> Starlight-Cadence_airegin)
    (S -> Starlight-Cadence_tension)
    (S -> Starlight-Cadence_Night-&-Day)
    (S -> Starlight-Dropback) (S -> Starlight-Opening)
    (S -> Straight-Approach_main)
    (S -> Straight-Approach_var)
    (S -> Straight-Cadence_main)
    (S -> Straight-Cadence-with-Dropback)
    (S -> Straight-Cadence-+-Dominant-Overrun)
    (S -> Surge_major)
    (S -> Surge_minor)
    (S -> Surge_dominant)
    (S -> Surge_majorvar)
    (S -> Surge_minorvar)
    (S -> Surge_dominantvar)
    (S -> Supertension-Ending)
    (S -> Surprise-Minor-Cadence)
    (S -> Surprise-Major-Cadence)
    (S -> Sus-Approach)
    (S -> Sus-Approach_var1)
    (S -> Sus-Cadence)
    (S -> Sus-Cadence_main)
    (S -> Sus-Cadence_var1)
    (S -> Sus-Cadence_var2)
    (S -> ,Tension-Cadence)
    (S -> ,Tension-Pullback)
    (S -> ,Tension-SPOT)
    (S -> ,To-II-n-Back)
    (S -> ,To-Somewhere)
    (S -> ,Cadence_perfect)
    (S -> ,Cadence_straight)
    (S -> ,Cadence_sad)
    (S -> ,Cadence_tritone)
    (S -> ,Cadence_surge)
    (S -> ,To-IV-n-Bird-POT)
    (S -> ,TTFA-Dropback_main)
    (S -> ,TTFA-Dropback_dropback)
    (S -> ,TTFA-Dropback_IV-variant)
    (S -> ,TTFA-Dropback_ii-variant)
    (S -> ,TTFA-Dropback_IInBackvariant)
    (S -> ,TTFA-Dropback_rainy)
    (S -> ,TTFA-Dropback_minorchromaticdescent)
    (S -> ,Twopenny-Approach)
    (S -> ,Twopenny-Ending_main)
    (S -> ,Twopenny-Ending_Nowheredropback)
    (S -> ,Twopenny-Ending_Starlightback)
    (S -> ,Twopenny-Ending_var1)
    (S -> ,Twopenny-Ending_var2)
    (S -> ,Twopenny-Ending_var3)
    (S -> ,Upslide)
    (S -> ,Tritone-Approach)
    (S -> ,Tritone-Cadence_main)
    (S -> ,Tritone-Cadence_short)
    (S -> ,Tritone/Straight-Approach)
    (S -> ,Two-Goes-Approach)
    (S -> ,Two-Goes-Nowhere-Approach)
    (S -> ,Two-Goes-Nowhere-Cadence)
    (S -> ,Two-Goes-Pullback)
    (S -> ,Two-Goes-Pullback-Extended)
    (S -> ,Two-Goes-Pullback+Cadence)
    (S -> ,Two-Goes-Rainy-Turnaround)
    (S -> ,Two-Goes-Sad-Approach)
    (S -> ,Two-Goes-Sad-Cadence)
    (S -> ,Two-Goes-Starlight-Approach)
    (S -> ,Two-Goes-Straight-Approach)
    (S -> ,Two-Goes-Straight-Cadence)
    (S -> ,Two-Goes-Straight-Cadence-with-Overrun_var1)
    (S -> ,Two-Goes-Straight-Cadence-with-Overrun_var2)
    (S -> ,Three-Goes-Pullback)
    (S -> ,Three-Goes-Pullback-Extended)
    (S -> ,Three-Goes-Pullback+Cadence)
    (S -> ,Three-Goes-Sad-Approach)
    (S -> ,Three-Goes-Sad-Cadence)
    (S -> ,Three-Goes-Straight-Approach)
    (S -> ,Three-Goes-Straight-Cadence)
    (S -> ,Three-Goes-Nowhere-Approach)
    (S -> ,Three-Goes-Nowhere-Cadence)
    (S -> ,Four-Goes-Straight-Approach)
    (S -> ,Four-Goes-Straight-Cadence)
    (S -> ,Four-Goes-Sad-Approach)
    (S -> ,Four-Goes-Sad-Cadence)
    (S -> ,Two-Goes-Tritone-Approach)
    (S -> ,Two-Goes-Tritone-Cadence)
    (S -> ,Three-Goes-Tritone-Approach)
    (S -> ,Three-Goes-Tritone-Cadence)
    (S -> ,Four-Goes-Tritone-Approach)
    (S -> ,Four-Goes-Tritone-Cadence)
    (S -> ,Two-Goes-Dominant-Approach)
    (S -> ,Two-Goes-Tritone-Straight-Approach)
    (S -> ,Two-Goes-Tritone-Straight-Cadence)
    (S -> ,Whoopee-Turnaround)
    (S -> ,Whoopee-Cadence)
    (S -> ,Wonderful-Opening)
    (S -> ,Yardbird-Approach)
    (S -> ,Yardbird-Cadence_main)
    (S -> ,Yardbird-Cadence_var)
    (S -> ,Yardbird-Sub-Cadence)
    (S -> ,Yardbird-Sub-Turnaround)
    (S -> ,Major-On)
    (S -> ,Minor-On_main)
    (S -> ,Minor-On_var1)
    (S -> ,Minor-On_var2)
    (S -> ,Minor-On_var3)
    (S -> ,On-Off-Major-V)
    (S -> ,On-Off-Major-bII)
    (S -> ,On-Off-Major-Somewhere)
    (S -> ,On-Off-Major-bIII)
    (S -> ,On-Off-Major-III)
    (S -> ,On-Off-Major-IV)
    (S -> ,On-Off-Major-$IV)
    (S -> ,On-Off-Major-Nowhere)
    (S -> ,On-Off-Major-VI)
    (S -> ,On-Off-Major-bVII)
    (S -> ,On-Off-Major-VII)
    (S -> ,On-Off-Minor-bII)
    (S -> ,On-Off-Minor-Somewhere)
    (S -> ,On-Off-Minor-bIII)
    (S -> ,On-Off-Minor-III)
    (S -> ,On-Off-Minor-IV)
    (S -> ,On-Off-Minor-$IV)
    (S -> ,On-Off-Minor-V)
    (S -> ,On-Off-Minor-Nowhere)
    (S -> ,On-Off-Minor-VI)
    (S -> ,On-Off-Minor-bVII)
    (S -> ,On-Off-Minor-VII)
    (S -> ,Off-On-Major-bII)
    (S -> ,Off-On-Major-Somewhere)
    (S -> ,Off-On-Major-bIII)
    (S -> ,Off-On-Major-III)
    (S -> ,Off-On-Major-IV)
    (S -> ,Off-On-Major-$IV)
    (S -> ,Off-On-Major-Nowhere)
    (S -> ,Off-On-Major-VI)
    (S -> ,Off-On-Major-bVII)
    (S -> ,Off-On-Major-VII)
    (S -> ,Off-On-Minor-bII)
    (S -> ,Off-On-Minor-Somewhere)
    (S -> ,Off-On-Minor-bIII)
    (S -> ,Off-On-Minor-III)
    (S -> ,Off-On-Minor-IV)
    (S -> ,Off-On-Minor-$IV)
    (S -> ,Off-On-Minor-Nowhere)
    (S -> ,Off-On-Minor-VI)
    (S -> ,Off-On-Minor-bVII)
    (S -> ,Off-On-Minor-VII)
    (S -> ,On-Off-Twice-To-V)
    (S -> ,On-Off-On-To-IV-Major)
    (S -> ,On-Off-On-+Dropback-IV-Major)
    (S -> ,On-Off-On-To-IV-Minor)
    (S -> ,On-Off-Twice-On-To-IV)
    (S -> ,On-Off-Thrice-To-IV)
    (S -> ,On-Off-Thrice-On-To-IV)
    (S -> ,On-Off-Four-Times-On-To-IV)
    (S -> ,On-Off-On-To-V_main)
    (S -> ,On-Off-On-To-V_minor)
    (S -> ,On-Off-On-To-V_sus)
    (S -> ,On-Off-Twice-On-To-V)
    (S -> ,On-Off-Thrice-On-To-V)
    (S -> ,On-Off-Four-Times-On-To-V)
    (S -> ,On-Off-On-To-II)
    (S -> ,On-Off-Twice-To-II)
    (S -> ,On-Off-Twice-On-To-II)
    (S -> ,On-Off-Thrice-On-To-II)
    (S -> ,On-Off-Four-Times-On-To-II)
    (S -> ,On-Off-To-IV-Twice-Major-then-Minor)
    (S -> ,On-Off-Major-Minor-V)
    (S -> ,On-Off-Twice-Minor-V)
    (S -> ,On-Off-Thrice-Minor-V)
    (S -> ,On-Off-On-Minor-V)
    (S -> ,On-Off-On-Twice-Minor-V)
    (S -> ,On-Off-On-To-bVII)
    (S -> ,On-Off-Twice-On-To-bVII)
    (S -> ,On-Off-Thrice-On-To-bVII)
    (S -> ,On-Off-Four-Times-On-To-bVII)
    (S -> ,Diatonic-I-ii-iii)
    (S -> ,Diatonic-I-ii-iii-vi)
    (S -> ,On-Off-to-bVII)
    (S -> ,On-Off-Twice-to-bVII)
    (S -> ,To-IV-n-Back)
    (S -> ,To-IV-n-Back_blues)
    (S -> ,IV-n-Back)
    (S -> ,IV-n-Back_blues)
    (S -> ,Diatonic-Walk-Down)
    (S -> ,Minor-ii-to-Minor-v)
    (S -> ,To-IV-+-Bird-Approach)
    (S -> ,Daahoud-Cadence)
    (S -> ,Rainy-Pullback-Extended)
    (S -> ,Rainbow-Overrun)
    (S -> ,Rainbow-Overrun_sub1)
    (S -> ,Major-Off-On_ii-I)
    (S -> ,Major-Off-On_iv-I)
    (S -> ,Major-Off-On-Off)
    (S -> ,Diatonic-ii-I)
    (S -> ,Approach-from-iii)
    (S -> ,Four-Star-Ending)
    (S -> ,Four-Star-Ending_var1)
    (S -> ,Major-On-+-Dropback)
    (S -> ,Descending-Minor-CESH-Approach)
    (S -> ,On-+-Dropback)
    (S -> ,Tritone-Sub-Turnaround)
    (S -> Somewhere-Turnaround)
    (S -> ,Giant-Step-Turnaround_var1)
    (S -> ,Giant-Step-Turnaround_var2)
    (S -> ,Seven-sus4-to-3)))

(define rules (append brick-rules S-rules))

(define bricks (map car brick-rules))

(define chords
  (let ([blocks (map cddr rules)])
    (let ([blocks (map remv-dups blocks)])
      (difference (apply union blocks) bricks))))

(define fake-start-symbol 'S)
(define nonterminals (cons fake-start-symbol bricks))

(define bricks-cfg
  (make-cfg ,nonterminals ,chords ,rules ,fake-start-symbol))

(define normalized-bricks-cfg (normalize bricks-cfg))

;;; sanity check
(verify-cnf normalized-bricks-cfg)



#!eof

; old and busted
(define brick-rules
  '((GenMinor -> GenMinor_var1)
    (GenMinor_var1 -> Cm)
    (GenMinor_var2 -> Cm7)
    (GenDom -> GenDom_var1)
    (GenDom_var1 -> G7)
    (GenDom_var2 -> GenII G7)
    (GenII -> GenII_var1)
    (GenII_var1 -> Dm7)
    (GenII_var2 -> Dm7b5)
    (LiberalII -> LiberalII_var1)
    (LiberalII_var1 -> GenII)
    (LiberalII_var2 -> D7)
    (GenIII -> GenIII_var1)
    (GenIII_var1 -> Em7)
    (GenIII_var2 -> E7)
    (GenIII_var3 -> Em7b5)
    (GenVI -> GenVI_var1)
    (GenVI_var1 -> Am7)
    (GenVI_var2 -> A7)
    (GenVI_var3 -> Am7b5)
    (Gen7 -> Gen7_var1)
    (Gen7_var1 -> C7)
    (Gen7_var2 -> Cm7)
    (An-Approach -> GenII G7)
    (Liberal-Approach -> Liberal-Approach_var1)
    (Liberal-Approach_var1 -> An-Approach)
    (Liberal-Approach_var2 -> GenII Gm7)
    (SuperGenDom -> SuperGenDom_var1)
    (SuperGenDom_var1 -> G7)
    (SuperGenDom_var2 -> GenII G7)
    (SuperGenDom_var3 -> GenII Db7)
    (GenMajorTonic -> GenMajorTonic_main)
    (GenMajorTonic_main -> C)
    (GenMajorTonic_iii -> Em7)
    (Off -> Off_dominant)
    (Off_dominant -> C7)
    (Off_major -> C)
    (Off_minor -> Cm)
    (Off_minor7 -> Cm7)
    (Off_minor-major -> CmM7)
    (Off_diminished -> Co7)
    (TonicOrDom -> TonicOrDom_major)
    (TonicOrDom_major -> C)
    (TonicOrDom_minor -> Cm)
    (TonicOrDom_dom -> C7)
    (MajorOrDom -> MajorOrDom_major)
    (MajorOrDom_major -> C)
    (MajorOrDom_dom -> C7)
    (DomOrMinor -> DomOrMinor_minor)
    (DomOrMinor_minor -> Gm)
    (DomOrMinor_minor7 -> Gm7)
    (DomOrMinor_dom -> G7)
    (Ascending-Minor-CESH -> Ascending-Minor-CESH_3step)
    (Ascending-Minor-CESH_3step -> Cm Cm+ Cm6)
    (Ascending-Minor-CESH_4step -> Cm Cm+ Cm6 Cm7)
    (Ascending-Minor-CESH-Approach -> Cm Cm+ Cm6 C7)
    (Descending-Minor-CESH -> Descending-Minor-CESH_3step)
    (Descending-Minor-CESH_3step -> Cm CmM7 Cm7)
    (Descending-Minor-CESH_4step
     ->
     Descending-Minor-CESH_3step
     Cm6)
    (Ascending-Major-CESH -> Ascending-Major-CESH_3step)
    (Ascending-Major-CESH_3step -> C C+ C6)
    (Ascending-Major-CESH_4step -> C C+ C6 C7)
    (Ascending-&-Descending-Minor-CESH
     ->
     Ascending-&-Descending-Minor-CESH_4step)
    (Ascending-&-Descending-Minor-CESH_4step -> Cm Cm+ Cm6 Cm+)
    (Ascending-&-Descending-Minor-CESH_5step -> Cm Cm+ Cm6 Cm7
                                             Cm6)
    (Ascending-&-Descending-Major-CESH -> Major-On AbM7$5
                                       Major-On AbM7$5 AbM7)
    (Seven-Chord-Dropback -> Seven-Chord-Dropback_main)
    (Seven-Chord-Dropback_main -> Dropback-Approach C)
    (Seven-Chord-Dropback_minoron
     ->
     Minor-On
     Perfect-Cadence
     Long-Cadence)
    (Seven-Chord-Dropback_rainy -> Dm7 G7 C Ebo Dm7 G7 C)
    (Amen-Cadence -> TonicOrDom C)
    (Autumn-Leaves-Opening -> Autumn-Leaves-Opening_main)
    (Autumn-Leaves-Opening_main
     ->
     Straight-Approach
     C
     Sad-Cadence)
    (Autumn-Leaves-Opening_withoverrun -> Straight-Approach C F
                                       Sad-Cadence)
    (Autumnal-Approach -> Autumnal-Approach_var1)
    (Autumnal-Approach_var1 -> An-Approach An-Approach)
    (Autumnal-Approach_var2 -> Minor-On An-Approach)
    (Autumnal-Cadence -> Autumnal-Cadence_main)
    (Autumnal-Cadence_main -> Autumnal-Approach GenMinor)
    (Autumnal-Cadence_var1 -> GenII Sad-Cadence)
    (Autumnal-Cadence_var2 -> GenMinor Cadence)
    (Bemsha-Turnaround-1 -> C Eb7 D7 Db7)
    (Bemsha-Turnaround-2 -> C Ab7 DbM7 Gb7)
    (Bemsha-Turnaround-3 -> C Bb7 EbM7 Ab7)
    (Body-&-Soul-Approach -> Body-&-Soul-Approach_main)
    (Body-&-Soul-Approach_main -> Dm7 GenDom An-Approach)
    (Body-&-Soul-Approach_var1 -> Dm A7 Ab7 GenDom)
    (Body-&-Soul-Approach_var2
     ->
     On-Off-Minor-Somewhere
     Straight-Approach)
    (Body-&-Soul-Cadence -> Body-&-Soul-Cadence_major)
    (Body-&-Soul-Cadence_major -> Body-&-Soul-Approach C)
    (Body-&-Soul-Cadence_minor -> Body-&-Soul-Approach Cm)
    (Chromatic-Dropback -> Chromatic-Dropback_main)
    (Chromatic-Dropback_main -> C B7 Bb7 A7)
    (Chromatic-Major-Walkup -> Major-On On-Off-Major-bII)
    (Chromatic-Minor-Ascending
     ->
     Chromatic-Minor-Ascending_3steps)
    (Chromatic-Minor-Ascending_3steps -> GenII GenII GenII)
    (Chromatic-Minor-Ascending_4steps
     ->
     Chromatic-Minor-Ascending_3steps
     GenII)
    (Chromatic-Minor-Descending
     ->
     Chromatic-Minor-Descending_3steps)
    (Chromatic-Minor-Descending_3steps -> Dm7 Dbm7 Cm7)
    (Chromatic-Minor-Descending_4steps
     ->
     Chromatic-Minor-Descending_3steps
     Cm7)
    (Chromatically-Descending-Dominants
     ->
     Chromatically-Descending-Dominants_3steps)
    (Chromatically-Descending-Dominants_3steps -> A7 Ab7 G7)
    (Chromatically-Descending-Dominants_4steps
     ->
     Bb7
     Chromatically-Descending-Dominants_3steps)
    (Chromatically-Descending-Dominants_5steps
     ->
     B7
     Chromatically-Descending-Dominants_4steps)
    (Chromatically-Descending-Dominants_6steps
     ->
     C7
     Chromatically-Descending-Dominants_5steps)
    (Coltrane-Cadence -> Dm7 Perfect-Cadence Perfect-Cadence
                      Perfect-Cadence)
    (Cyclic-Approach -> Cyclic-Approach_2)
    (Cyclic-Approach_2 -> Dm7 D7 Gm7 G7)
    (Cyclic-Approach_3 -> Cyclic-Approach_2 Gm7 G7)
    (Diatonic-Walkup -> Diatonic-Walkup_3stepsfromii)
    (Diatonic-Walkup_3stepsfromii -> Dm7 Em7 Fm7)
    (Diatonic-Walkup_4stepsfromii -> Dm7 Em7 Fm7 Gb7)
    (Diatonic-I-ii-iii-ii -> C Dm7 Em7 Dm7)
    (Diatonic-I-ii-iii-IV -> C Dm7 Em7 F)
    (Diatonic-I-ii-iii-biii -> C Dm7 Em7 Ebm7)
    (Diatonic-I-ii-iii-IV-V-vi -> C Dm7 Em7 F G7 Am7)
    (Diatonic-I-IV-iii-ii -> C F Em7 Dm7)
    (Diatonic-ii-iii-IV -> Dm7 Em7 F)
    (Diatonic-ii-iii-IV-V -> Diatonic-ii-iii-IV G7)
    (Diatonic-ii-iii-IV-iii-ii -> Dm7 Em7 F Em7 Dm7)
    (Dizzy-Approach -> Dizzy-Approach_var1)
    (Dizzy-Approach_var1 -> Dm Straight-Approach)
    (Dizzy-Approach_var2 -> Dm7b5 Db7)
    (Dizzy-Cadence -> Dizzy-Cadence_main)
    (Dizzy-Cadence_main -> Dizzy-Approach C)
    (Dizzy-Cadence_var1 -> G7 Tritone-Cadence)
    (Dizzy-Cadence_var2 -> Nowhere-Approach C)
    (Dogleg-Approach -> Dogleg-Approach_short)
    (Dogleg-Approach_short -> D7 Dm7 G7)
    (Dogleg-Approach_long
     ->
     Straight-Approach
     Straight-Approach)
    (Dogleg-Cadence -> Dogleg-Approach C)
    (Dogleg-Cycle -> Dogleg-Cycle_3steps)
    (Dogleg-Cycle_3steps
     ->
     Straight-Approach
     Straight-Approach
     Straight-Approach)
    (Dogleg-Cycle_4steps
     ->
     Straight-Approach
     Dogleg-Cycle_3steps)
    (Dogleg-Cycle_5steps
     ->
     Straight-Approach
     Dogleg-Cycle_4steps)
    (Dogleg-Cycle_6steps
     ->
     Straight-Approach
     Dogleg-Cycle_5steps)
    (Dominant-Cycle-2-Steps -> D7 G7)
    (Dominant-Cycle-3-Steps -> A7 Dominant-Cycle-2-Steps)
    (Dominant-Cycle-4-Steps -> E7 Dominant-Cycle-3-Steps)
    (Dominant-Cycle-5-Steps -> B7 Dominant-Cycle-4-Steps)
    (Dominant-Cycle-6-Steps -> F$7 Dominant-Cycle-5-Steps)
    (Dominant-Cycle-7-Steps -> Db7 Dominant-Cycle-6-Steps)
    (Dominant-Cycle-8-Steps -> Ab7 Dominant-Cycle-7-Steps)
    (Dominant-Cycle -> Dominant-Cycle_step-2)
    (Dominant-Cycle_step-2 -> Dominant-Cycle-2-Steps)
    (Dominant-Cycle_step-3 -> Dominant-Cycle-3-Steps)
    (Dominant-Cycle_step-4 -> Dominant-Cycle-4-Steps)
    (Dominant-Cycle_step-5 -> Dominant-Cycle-5-Steps)
    (Dominant-Cycle_step-6 -> Dominant-Cycle-6-Steps)
    (Dominant-Cycle_step-7 -> Dominant-Cycle-7-Steps)
    (Dominant-Cycle_step-8 -> Dominant-Cycle-8-Steps)
    (Dominant-Cycle-Cadence -> Dominant-Cycle Major-On)
    (Dominant-Turnaround -> C7 Ab7 G7 C7)
    (Donna-Lee-Start -> Donna-Lee-Start_var1)
    (Donna-Lee-Start_var1 -> C Straight-Approach)
    (Donna-Lee-Start_var2 -> C GenDom)
    (Donna-Lee-Opening -> Donna-Lee-Opening_main)
    (Donna-Lee-Opening_main -> Donna-Lee-Start Straight-Cadence)
    (Donna-Lee-Opening_Dizzycadenceending
     ->
     Donna-Lee-Start
     Tritone-Cadence)
    (Doo-Wop -> C Am F G)
    (Double-Pullback -> An-Approach An-Approach An-Approach)
    (Dropback -> Dropback_main)
    (Dropback_main -> C A7)
    (Dropback_iii-VI-var -> C Em7 A7)
    (Dropback_tritone -> C Bb7 A7)
    (Dropback_chromatic -> C B7 Bb7 A7)
    (Dropback_TINGLe -> C TonicOrDom Nowhere-Approach)
    (Dropback-Approach -> Dropback-Approach_main)
    (Dropback-Approach_main
     ->
     Straight-Cadence
     Extended-Approach)
    (Extended-Approach -> Extended-Approach_var1)
    (Extended-Approach_var1 -> GenVI Straight-Approach)
    (Extended-Cadence -> Extended-Approach C)
    (Foggy-Cadence -> Eb7 Straight-Approach C)
    (Foggy-Turnaround -> Foggy-Turnaround_main)
    (Foggy-Turnaround_main -> C Eb7 Straight-Approach)
    (Foggy-Turnaround_var -> C Eb7 D7 G7)
    (Foolish-Approach -> Raindrop Straight-Approach)
    (Four-Star-Approach -> F Straight-Approach Long-Approach)
    (GDS-Cadence -> D/C Db/C C)
    (Giant-Step -> Major-On Perfect-Cadence)
    (Giant-Steps -> Giant-Step Perfect-Cadence)
    (Giant-Step-Approach -> Ab7 Major-On E7)
    (Happenstance-Cadence -> An-Approach C)
    (Honeysuckle-Bridge -> Honeysuckle-Bridge_main)
    (Honeysuckle-Bridge_main
     ->
     Straight-Cadence
     Straight-Approach
     Straight-Approach)
    (Honeysuckle-Bridge_two-goes -> Two-Goes-Straight-Approach F
                                 Two-Goes-Straight-Approach G7)
    (Honeysuckle-Bridge_two-goes-variant
     ->
     Straight-Approach
     Honeysuckle-Bridge)
    (Honeysuckle-Bridge_dogleg -> Straight-Cadence Dogleg-Cycle)
    (Honeysuckle-Bridge_two-goes-dogleg
     ->
     Two-Goes-Straight-Approach
     Dropback
     An-Approach)
    (Honeysuckle-Bridge_pullback
     ->
     Pullback-to-Cadence
     Pullback
     Dominant-Cycle-2-Steps)
    (Honeysuckle-Bridge_supertensionend
     ->
     Straight-Cadence
     Tension-Cadence)
    (II-n-Back -> II-n-Back_main)
    (II-n-Back_main -> Dm7 D$o C/E)
    (II-n-Back_var -> Dm7 D$o Em)
    (II-n-Bird-Approach -> Minor-On Yardbird-Approach)
    (II-n-Bird-POT -> Dm7 GenDom POT)
    (II-n-Bird-SPOT -> II-n-Bird-SPOT_main)
    (II-n-Bird-SPOT_main -> Dm7 GenDom SPOT)
    (II-n-Bird-SPOT_var1
     ->
     Minor-On
     Straight-Approach
     Rainy-Turnaround)
    (To-IV -> To-IV_main)
    (To-IV_main -> Major-On Cadence)
    (IV-n-Yak -> MajorOrDom GenDom Major-On)
    (To-IV-n-Yak -> Major-On SuperGenDom IV-n-Yak)
    (To-IV-n-Yak-Turnaround -> Major-On SuperGenDom MajorOrDom
                            GenDom)
    (IV-n-Yak-To -> IV-n-Yak C7)
    (IV-n-Hack -> TonicOrDom Happenstance-Cadence)
    (To-IV-n-Hack -> Major-On SuperGenDom IV-n-Hack)
    (IV-n-Mack -> Major-On Minor-Plagal-Cadence)
    (To-IV-n-Mack -> To-IV Minor-Plagal-Cadence)
    (To-IV-n-Mack-Turnaround -> To-IV Minor-On)
    (IV-n-Bird -> TonicOrDom GenDom Minor-On)
    (To-IV-n-Bird -> Major-On SuperGenDom IV-n-Bird)
    (To-IV-n-Back-SPOT -> FM7 F7 Surge SPOT)
    (IV-n-Bird-SPOT -> IV-n-Bird-SPOT_main)
    (IV-n-Bird-SPOT_main -> TonicOrDom GenDom SPOT)
    (IV-n-Bird-SPOT_rainy -> IV-n-Bird Rainy-Turnaround)
    (To-IV-n-Bird-SPOT -> Major-On SuperGenDom IV-n-Bird-SPOT)
    (IV-n-Bauble -> Upslide Sad-Approach)
    (ITCHY-Opening -> C An-Approach Dm An-Approach)
    (La-Bomba -> C F G F)
    (Ladybird-Turnaround -> Ladybird-Turnaround_main)
    (Ladybird-Turnaround_main -> Major-On TonicOrDom TonicOrDom
                              TonicOrDom)
    (Ladybird-Cadence -> Ladybird-Turnaround C)
    (Light-&-Day-Approach -> Dm7b5 Dbm)
    (Lonely-Approach -> Straight-Approach Straight-Approach)
    (Lonely-Cadence -> Lonely-Approach C)
    (Long-Approach -> Long-Approach_main)
    (Long-Approach_main -> GenIII Extended-Approach)
    (Long-Approach_tritone -> Dizzy-Approach Abm7 Db7)
    (Long-Approach_sus -> Surprise-Minor-Cadence F/G)
    (Long-Cadence -> Long-Cadence_main)
    (Long-Cadence_main -> Long-Approach C)
    (Minor-Chromatic-Walkdown -> Minor-Chromatic-Walkdown_2step)
    (Minor-Chromatic-Walkdown_2step -> Dm7 Dbm7)
    (Minor-Chromatic-Walkdown_3step
     ->
     Minor-Chromatic-Walkdown_2step
     Dbm7)
    (Minor-Chromatic-Walkdown_4step
     ->
     Minor-Chromatic-Walkdown_3step
     G7)
    (Minor-Chromatic-Walkdown-Approach
     ->
     Minor-Chromatic-Walkdown
     G7)
    (Minor-Dropback -> Cm Am7b5)
    (Minor-Plagal-Cadence -> Minor-On Major-On)
    (Minor-Perfect-Cadence -> G7 Cm7)
    (Moment -> Moment_quotes-Cadence)
    (Moment_quotes-Cadence -> Moment_quotes-Approach C)
    (Moment_quotes-Approach
     ->
     Straight-Approach
     Straight-Approach)
    (Night-&-Day-Cadence -> AbM7 G7 C)
    (Nobody -> Nobody_quotes-Cadence)
    (Nobody_quotes-Cadence -> Eb G7 Cm)
    (Nowhere-Approach -> Nowhere-Approach_main)
    (Nowhere-Approach_main -> Ab7 GenDom)
    (Nowhere-Approach_slow
     ->
     Straight-Approach
     Nowhere-Approach)
    (Nowhere-Cadence -> Nowhere-Approach C)
    (Nowhere-Minor-Cadence -> Nowhere-Minor-Cadence_main)
    (Nowhere-Minor-Cadence_main -> Nowhere-Approach Cm)
    (Nowhere-Minor-Cadence_var1
     ->
     Dominant-Cycle-2-Steps
     Minor-On
     Cm7/Bb)
    (Nowhere-Turnaround -> Dropback Nowhere-Approach)
    (Nowhere-Turnaround-Minor
     ->
     Minor-Dropback
     Nowhere-Approach)
    (Nowhere-Turnaround+On -> Nowhere-Turnaround C)
    (Nowhere-Turnaround-to-Minor-On
     ->
     Nowhere-Turnaround-Minor
     Cm)
    (Passacaglia -> On-Off-Major-bVII On-Off-Major-VII)
    (POT-Spring-sub -> C Am7 Dm7 Straight-Approach)
    (POT+On -> POT+On_cyclic)
    (POT+On_cyclic -> POT_cyclic C)
    (POT_main -> C GenII Straight-Approach)
    (POT_var1 -> C A7 Straight-Approach)
    (POT_var2 -> On-Off-Major-VI Sad-Approach)
    (POT_var3 -> Major-On Straight-Approach)
    (POT_var4 -> C Am Am/C Dm7/G G7)
    (POT_var5 -> Dropback Minor-On Gsus)
    (POT_blues -> C7 A7 Dm7 G7)
    (POT_cyclic -> C A7 D7 G7)
    (POT_Yardbird -> C A7 Dm7 GenDom)
    (POT_sadapproach -> C A7 Sad-Approach)
    (POT -> C7 A7 Straight-Approach)
    (Minor-POT -> Minor-POT_main)
    (Minor-POT_main -> Minor-Dropback Sad-Approach)
    (Minor-POT_var1 -> Minor-Dropback D7 G7)
    (Minor-POT_var2 -> Cm Eb7 Dm7b5 G7)
    (Minor-POT_var3 -> Minor-Dropback Straight-Approach)
    (Minor-POT_dominant -> Cm Extended-Approach)
    (Minor-POT_sad -> Cm Sad-Approach)
    (Minor-POT_altereddominant -> Cm A7$11 D7 G7+)
    (Minor-POT+On -> Minor-POT Cm)
    (Pennies-Approach -> Pennies-Approach_main)
    (Pennies-Approach_main -> F Bb7 Dropback An-Approach)
    (Pennies-Approach_var1 -> F Bb7 GenMajorTonic An-Approach
                           An-Approach)
    (Pennies-Approach_var2
     ->
     IV-n-Yak
     Minor-Perfect-Cadence
     Dominant-Cycle-2-Steps)
    (Pennies-Ending -> Pennies-Ending_main)
    (Pennies-Ending_main -> Pennies-Approach C)
    (Pennies-Ending_raindrop -> Dm Raindrop Straight-Approach C)
    (Pennies-Ending_side-slipping -> Major-On Nowhere-Approach
                                  Stablemates-Cadence Straight-Approach)
    (Pennies-Ending_dropback -> IV-n-Yak A7 Cyclic-Approach C)
    (Pennies-Ending_Somewhere -> IV-n-Yak D7 Straight-Cadence)
    (Pennies-Ending_Yardbird
     ->
     IV-n-Yak
     Minor-Perfect-Cadence
     Straight-Cadence)
    (Pennies-Turnaround -> Pennies-Turnaround_main)
    (Pennies-Turnaround_main -> C Dm Raindrop Straight-Approach)
    (Pennies-Turnaround_TTFA -> TTFA-Dropback Straight-Approach)
    (Pennies-Turnaround-Two-Goes -> C Dm Raindrop
                                 Straight-Approach Straight-Approach)
    (Perfect-Cadence -> G7 C)
    (Pullback -> Pullback_basic)
    (Pullback_basic -> An-Approach An-Approach)
    (Pullback_var1 -> An-Approach Em7 Am7)
    (Pullback_tritone -> An-Approach Bb7 A7)
    (Rainy-Pullback -> Straight-Approach Raindrop)
    (Pullback-Extended -> Pullback-Extended_main)
    (Pullback-Extended_main -> Pullback An-Approach)
    (Pullback-Extended_yardbird -> Dm7 Bb7 Em7 A7 Dm7 G7)
    (Pullback-to-Cadence -> Pullback-Extended C)
    (Pullback-Cadence-with-Dropback
     ->
     Pullback-Cadence-with-Dropback_rainy)
    (Pullback-Cadence-with-Dropback_rainy
     ->
     Straight-Approach
     Seven-Chord-Dropback_rainy)
    (Rainbow-Cadence -> Rainbow-Cadence_var1)
    (Rainbow-Cadence_var1 -> G Bm C)
    (Rainbow-Cadence_var2 -> G/B B+ C7$11)
    (Rainbow-Cadence_var3 -> Major-On B+ Major-On)
    (Rainbow-Cadence_var4 -> G B7 C)
    (Raindrop -> Raindrop_main)
    (Raindrop_main -> Em Ebdim)
    (Raindrop_var1 -> C Ebdim)
    (Raindrop_var2 -> A7/E Ebdim)
    (Rainy-Turnaround -> Raindrop Straight-Approach)
    (Rainy-Cadence -> Rainy-Turnaround C)
    (Rainy-Cadence_minorchromatic
     ->
     Minor-Chromatic-Walkdown
     Straight-Cadence)
    (Reverse-Dominant-Cycle-2-Steps -> G7 D7)
    (Reverse-Dominant-Cycle-3-Steps
     ->
     Reverse-Dominant-Cycle-2-Steps
     A7)
    (Reverse-Dominant-Cycle-4-Steps
     ->
     Reverse-Dominant-Cycle-3-Steps
     E7)
    (Reverse-Dominant-Cycle-5-Steps
     ->
     Reverse-Dominant-Cycle-4-Steps
     B7)
    (Reverse-Dominant-Cycle-6-Steps
     ->
     Reverse-Dominant-Cycle-5-Steps
     F$7)
    (Rhythm-Bridge -> GenDom GenDom GenDom GenDom)
    (Rhythm-Turnaround -> Rhythm-Turnaround_main)
    (Rhythm-Turnaround_main -> Upslide Dm7 Ebo)
    (Rhythm-Turnaround_var -> Upslide G7/D Ebo)
    (Rhythm-Turnaround+On -> Dbdim Surge C)
    (Sad-Approach -> Dm7b5 G7)
    (Sad-Cadence -> Sad-Cadence_main)
    (Sad-Cadence_main -> Sad-Approach Cm)
    (Sad-Cadence_var -> F7$11 Minor-Perfect-Cadence)
    (Sad-Cadence-with-Overrun -> Sad-Cadence-with-Overrun_var)
    (Sad-Cadence-with-Overrun_var -> Sad-Cadence Ab7)
    (Sad-Dropback -> Dm7b5 G7 Cm Am7b5 Dm7b5 G7)
    (Sad-SPOT -> Sad-Approach Sad-Approach)
    (Satin-Cadence -> Straight-Approach Straight-Approach C)
    (Side-Slip -> C$m7b5 Cm7 F7)
    (Side-Slips -> Side-Slips_2)
    (Side-Slips_2 -> An-Approach An-Approach)
    (Side-Slips_2variant -> Em Am An-Approach)
    (Side-Slips_3 -> An-Approach Side-Slips_2)
    (Side-Slips_4 -> Side-Slips_2 Side-Slips_2)
    (Side-Slips_5 -> An-Approach Side-Slips_4)
    (Side-Slips_6 -> Side-Slips_2 Side-Slips_4)
    (Sharp-Fourpenny-Approach -> F$m7b5 Yardbird-Cadence
                              Straight-Approach Straight-Approach)
    (Sharp-Fourpenny-Ending -> Sharp-Fourpenny-Ending_main)
    (Sharp-Fourpenny-Ending_main -> Sharp-Fourpenny-Approach C)
    (Sharp-Fourpenny-Ending_var
     ->
     Side-Slip
     Straight-Approach
     Sus-Cadence)
    (Sixpenny-Approach -> Am Yardbird-Cadence Straight-Approach
                       Straight-Approach)
    (Sixpenny-Ending -> Sixpenny-Approach C)
    (Spring-Approach -> Spring-Approach_simple)
    (Spring-Approach_simple -> GenII Straight-Approach)
    (Spring-Approach_extended -> GenII E7 Straight-Approach)
    (Spring-Cadence -> Spring-Cadence_main)
    (Spring-Cadence_main -> Spring-Approach C)
    (Somewhere/Nowhere-Approach -> D7 Ab7)
    (SPOT -> SPOT_main)
    (SPOT_main -> An-Approach Straight-Approach)
    (SPOT_minorIV -> Em7 Extended-Approach)
    (SPOT_var1 -> Em7 Minor-Perfect-Cadence Straight-Approach)
    (SPOT_var2 -> Straight-Approach Straight-Approach G7/F)
    (SPOT_var3 -> Tritone-Approach Straight-Approach)
    (SPOT_sideslip -> Em7 Ebm7 Ab7 Dm7 G7)
    (Multi-Sub-POT -> C Bbm7 Eb7 Am7 D7 Abm7 Db7)
    (Stablemates-Approach -> Straight-Approach An-Approach)
    (Stablemates-Cadence -> Stablemates-Approach C)
    (Starlight-Approach -> Starlight-Approach_main)
    (Starlight-Approach_main
     ->
     An-Approach
     An-Approach
     An-Approach)
    (Starlight-Approach_Dizzy -> F$m7b5 B7 Tritone-Approach
                              Tritone-Approach)
    (Starlight-Approach_Dizzyvar2 -> Dizzy-Approach SPOT)
    (Starlight-Approach_Night-&-Day
     ->
     Light-&-Day-Approach
     SPOT)
    (Starlight-Approach_rainy -> F$m7b5 Fm Rainy-Turnaround)
    (Starlight-Approach_tritonestart -> Tritone-Approach SPOT)
    (Starlight-Cadence -> Starlight-Cadence_main)
    (Starlight-Cadence_main -> Starlight-Approach C)
    (Starlight-Cadence_airegin
     ->
     Sad-Approach
     Sad-Cadence
     Perfect-Cadence)
    (Starlight-Cadence_tension -> Starlight-Approach C7)
    (Starlight-Cadence_Night-&-Day
     ->
     Starlight-Approach_Night-&-Day
     C)
    (Starlight-Dropback -> Sad-Approach Straight-Approach)
    (Starlight-Opening -> Sad-Approach Straight-Approach)
    (Straight-Approach -> Straight-Approach_main)
    (Straight-Approach_main -> Dm7 G7)
    (Straight-Approach_var -> Gm7 C7 C7/Bb)
    (Straight-Cadence -> Straight-Cadence_main)
    (Straight-Cadence_main -> Straight-Approach C)
    (Straight-Cadence-with-Dropback -> Straight-Approach C Am7)
    (Straight-Cadence-+-Dominant-Overrun -> Straight-Cadence F7)
    (Surge -> Surge_major)
    (Surge_major -> Major-On C$o)
    (Surge_minor -> Minor-On C$o)
    (Surge_dominant -> C7 C$o)
    (Surge_majorvar -> Surge_major G/D)
    (Surge_minorvar -> Surge_minor G/D)
    (Surge_dominantvar -> Surge_dominant G/D)
    (Supertension-Ending -> Straight-Approach C7$4)
    (Surprise-Minor-Cadence -> Straight-Approach Cm)
    (Surprise-Major-Cadence -> Sad-Approach C)
    (Sus-Approach -> Major-On D7 Major-On)
    (Sus-Approach_var1 -> Minor-On Gsus)
    (Sus-Cadence -> Gm7 Gm7/C Major-On)
    (Sus-Cadence_main -> G7sus C)
    (Sus-Cadence_var1 -> Dm7 G7sus C)
    (Sus-Cadence_var2 -> G7sus G7 C)
    (Tension-Cadence -> Cm7 F7 Bb7)
    (Tension-Pullback
     ->
     Straight-Approach
     Dominant-Cycle-2-Steps)
    (Tension-SPOT -> Dominant-Cycle-2-Steps Straight-Approach)
    (To-II-n-Back -> C Minor-On Ebm7b5 Major-On)
    (To-Somewhere -> C D7)
    (Cadence -> Cadence_perfect)
    (Cadence_perfect -> Perfect-Cadence)
    (Cadence_straight -> Straight-Cadence)
    (Cadence_sad -> Sad-Cadence)
    (Cadence_tritone -> Tritone-Cadence)
    (Cadence_surge -> TonicOrDom F$o C)
    (To-IV-n-Bird-POT -> C GenDom FM7 Bb7 POT)
    (TTFA-Dropback -> TTFA-Dropback_main)
    (TTFA-Dropback_main -> C Straight-Approach)
    (TTFA-Dropback_dropback -> C/G Minor-Perfect-Cadence A7)
    (TTFA-Dropback_IV-variant -> C TonicOrDom Liberal-Approach)
    (TTFA-Dropback_ii-variant -> C Dm7 Straight-Approach)
    (TTFA-Dropback_IInBackvariant -> Major-On II-n-Back Am7)
    (TTFA-Dropback_rainy -> C F7 Em7 Ebo)
    (TTFA-Dropback_minorchromaticdescent
     ->
     C
     F7
     Minor-Chromatic-Walkdown)
    (Twopenny-Approach -> Dm Yardbird-Cadence Straight-Approach
                       Straight-Approach)
    (Twopenny-Ending -> Twopenny-Ending_main)
    (Twopenny-Ending_main -> Twopenny-Approach C)
    (Twopenny-Ending_Nowheredropback -> Yardbird-Sub-Cadence
                                     Abm7 Db7 Gm7 C7 FM7)
    (Twopenny-Ending_Starlightback -> Yardbird-Sub-Cadence Bm7b5
                                   Bb7 Am7 D7 Gm7 C7 FM7)
    (Twopenny-Ending_var1 -> Am7 GenDom Straight-Approach
                          Stablemates-Cadence)
    (Twopenny-Ending_var2 -> Dm7 B7 Em7 A7 An-Approach C)
    (Twopenny-Ending_var3 -> II-n-Bird-SPOT EbM7)
    (Upslide -> C Dbdim)
    (Tritone-Approach -> Dm7 Db7)
    (Tritone-Cadence -> Tritone-Cadence_main)
    (Tritone-Cadence_main -> Dm7 Db7 C)
    (Tritone-Cadence_short -> Db7 C)
    (Tritone/Straight-Approach
     ->
     Tritone-Approach
     Straight-Approach
     C7/Bb)
    (Two-Goes-Approach -> An-Approach An-Approach)
    (Two-Goes-Nowhere-Approach
     ->
     Nowhere-Approach
     Nowhere-Approach)
    (Two-Goes-Nowhere-Cadence -> Two-Goes-Nowhere-Approach C)
    (Two-Goes-Pullback -> Pullback_basic Pullback_basic)
    (Two-Goes-Pullback-Extended
     ->
     Two-Goes-Pullback
     Straight-Approach)
    (Two-Goes-Pullback+Cadence -> Two-Goes-Pullback-Extended C)
    (Two-Goes-Rainy-Turnaround
     ->
     Rainy-Turnaround
     Rainy-Turnaround)
    (Two-Goes-Sad-Approach -> Sad-Approach Sad-Approach)
    (Two-Goes-Sad-Cadence -> Two-Goes-Sad-Approach Cm)
    (Two-Goes-Starlight-Approach
     ->
     Straight-Approach
     Starlight-Approach)
    (Two-Goes-Straight-Approach
     ->
     Straight-Approach
     Straight-Approach)
    (Two-Goes-Straight-Cadence -> Two-Goes-Straight-Approach C)
    (Two-Goes-Straight-Cadence-with-Overrun
     ->
     Two-Goes-Straight-Cadence-with-Overrun_var1)
    (Two-Goes-Straight-Cadence-with-Overrun_var1
     ->
     Two-Goes-Straight-Cadence
     F)
    (Two-Goes-Straight-Cadence-with-Overrun_var2
     ->
     Two-Goes-Straight-Cadence
     F7)
    (Three-Goes-Pullback -> Two-Goes-Pullback Pullback)
    (Three-Goes-Pullback-Extended
     ->
     Three-Goes-Pullback
     Straight-Approach)
    (Three-Goes-Pullback+Cadence
     ->
     Three-Goes-Pullback-Extended
     C)
    (Three-Goes-Sad-Approach
     ->
     Two-Goes-Sad-Approach
     Sad-Approach)
    (Three-Goes-Sad-Cadence -> Three-Goes-Sad-Approach Cm)
    (Three-Goes-Straight-Approach
     ->
     Two-Goes-Straight-Approach
     Straight-Approach)
    (Three-Goes-Straight-Cadence
     ->
     Three-Goes-Straight-Approach
     C)
    (Three-Goes-Nowhere-Approach
     ->
     Two-Goes-Nowhere-Approach
     Nowhere-Approach)
    (Three-Goes-Nowhere-Cadence
     ->
     Three-Goes-Nowhere-Approach
     C)
    (Four-Goes-Straight-Approach
     ->
     Two-Goes-Straight-Approach
     Two-Goes-Straight-Approach)
    (Four-Goes-Straight-Cadence
     ->
     Four-Goes-Straight-Approach
     C)
    (Four-Goes-Sad-Approach
     ->
     Two-Goes-Sad-Approach
     Two-Goes-Sad-Approach)
    (Four-Goes-Sad-Cadence -> Four-Goes-Sad-Approach Cm)
    (Two-Goes-Tritone-Approach
     ->
     Tritone-Approach
     Tritone-Approach)
    (Two-Goes-Tritone-Cadence -> Two-Goes-Tritone-Approach C)
    (Three-Goes-Tritone-Approach
     ->
     Two-Goes-Tritone-Approach
     Tritone-Approach)
    (Three-Goes-Tritone-Cadence
     ->
     Three-Goes-Tritone-Approach
     C)
    (Four-Goes-Tritone-Approach
     ->
     Three-Goes-Tritone-Approach
     Tritone-Approach)
    (Four-Goes-Tritone-Cadence -> Four-Goes-Tritone-Approach C)
    (Two-Goes-Dominant-Approach
     ->
     Dominant-Cycle-2-Steps
     Dominant-Cycle-2-Steps)
    (Two-Goes-Tritone-Straight-Approach
     ->
     Tritone-Approach
     Straight-Approach)
    (Two-Goes-Tritone-Straight-Cadence
     ->
     Two-Goes-Tritone-Straight-Approach
     C)
    (Whoopee-Turnaround -> Upslide Straight-Approach)
    (Whoopee-Cadence -> Dbdim Straight-Cadence)
    (Wonderful-Opening -> Surge Straight-Cadence)
    (Yardbird-Approach -> Fm7 Bb7)
    (Yardbird-Cadence -> Yardbird-Cadence_main)
    (Yardbird-Cadence_main -> Yardbird-Approach C)
    (Yardbird-Cadence_var -> Bb7 C)
    (Yardbird-Sub-Cadence -> Dm7 GenDom C)
    (Yardbird-Sub-Turnaround -> C Minor-On Bb7)
    (Major-On -> C)
    (Minor-On -> Minor-On_main)
    (Minor-On_main -> Cm)
    (Minor-On_var1 -> CmM7)
    (Minor-On_var2 -> Cm6)
    (Minor-On_var3 -> Cm7)
    (On-Off-Major-V -> C G7)
    (On-Off-Major-bII -> Major-On Off)
    (On-Off-Major-Somewhere -> Major-On Off)
    (On-Off-Major-bIII -> Major-On Off)
    (On-Off-Major-III -> Major-On Off)
    (On-Off-Major-IV -> Major-On Off)
    (On-Off-Major-$IV -> Major-On Off)
    (On-Off-Major-Nowhere -> Major-On Off)
    (On-Off-Major-VI -> Major-On Off)
    (On-Off-Major-bVII -> Major-On Off)
    (On-Off-Major-VII -> Major-On Off)
    (On-Off-Minor-bII -> Minor-On Off)
    (On-Off-Minor-Somewhere -> Minor-On Off)
    (On-Off-Minor-bIII -> Minor-On Off)
    (On-Off-Minor-III -> Minor-On Off)
    (On-Off-Minor-IV -> Minor-On Off)
    (On-Off-Minor-$IV -> Minor-On Off)
    (On-Off-Minor-V -> GenMinor G7)
    (On-Off-Minor-Nowhere -> Minor-On Off)
    (On-Off-Minor-VI -> Minor-On Off)
    (On-Off-Minor-bVII -> Minor-On Off)
    (On-Off-Minor-VII -> Minor-On Off)
    (Off-On-Major-bII -> Off Major-On)
    (Off-On-Major-Somewhere -> Off Major-On)
    (Off-On-Major-bIII -> Off Major-On)
    (Off-On-Major-III -> Off Major-On)
    (Off-On-Major-IV -> Off Major-On)
    (Off-On-Major-$IV -> Off Major-On)
    (Off-On-Major-Nowhere -> Off Major-On)
    (Off-On-Major-VI -> Off Major-On)
    (Off-On-Major-bVII -> Off Major-On)
    (Off-On-Major-VII -> Off Major-On)
    (Off-On-Minor-bII -> Off Minor-On)
    (Off-On-Minor-Somewhere -> Off Minor-On)
    (Off-On-Minor-bIII -> Off Minor-On)
    (Off-On-Minor-III -> Off Minor-On)
    (Off-On-Minor-IV -> Off Minor-On)
    (Off-On-Minor-$IV -> Off Minor-On)
    (Off-On-Minor-Nowhere -> Off Minor-On)
    (Off-On-Minor-VI -> Off Minor-On)
    (Off-On-Minor-bVII -> Off Minor-On)
    (Off-On-Minor-VII -> Off Minor-On)
    (On-Off-Twice-To-V -> Major-On G7 Major-On G7)
    (On-Off-On-To-IV-Major -> Major-On Amen-Cadence)
    (On-Off-On-+Dropback-IV-Major
     ->
     On-Off-On-To-IV-Major
     SuperGenDom)
    (On-Off-On-To-IV-Minor -> Minor-On Fm Minor-On)
    (On-Off-Twice-On-To-IV
     ->
     On-Off-On-To-IV-Major
     Amen-Cadence)
    (On-Off-Thrice-To-IV
     ->
     On-Off-Major-IV
     On-Off-Major-IV
     On-Off-Major-IV)
    (On-Off-Thrice-On-To-IV
     ->
     On-Off-Twice-On-To-IV
     Amen-Cadence)
    (On-Off-Four-Times-On-To-IV
     ->
     On-Off-Thrice-On-To-IV
     Amen-Cadence)
    (On-Off-On-To-V -> On-Off-On-To-V_main)
    (On-Off-On-To-V_main -> Major-On Perfect-Cadence)
    (On-Off-On-To-V_minor -> Minor-On DomOrMinor Minor-On)
    (On-Off-On-To-V_sus -> Major-On Gsus Major-On)
    (On-Off-Twice-On-To-V -> On-Off-On-To-V Perfect-Cadence)
    (On-Off-Thrice-On-To-V
     ->
     On-Off-Twice-On-To-V
     Perfect-Cadence)
    (On-Off-Four-Times-On-To-V
     ->
     On-Off-Thrice-On-To-V
     Perfect-Cadence)
    (On-Off-On-To-II -> Major-On Dm7 Major-On)
    (On-Off-Twice-To-II -> Major-On Dm7 Major-On Dm7)
    (On-Off-Twice-On-To-II -> On-Off-On-To-II Dm7 Major-On)
    (On-Off-Thrice-On-To-II
     ->
     On-Off-Twice-On-To-II
     Dm7
     Major-On)
    (On-Off-Four-Times-On-To-II
     ->
     On-Off-Thrice-On-To-II
     Dm7
     Major-On)
    (On-Off-To-IV-Twice-Major-then-Minor
     ->
     On-Off-Major-IV
     On-Off-Minor-IV)
    (On-Off-Major-Minor-V -> On-Off-Major-V On-Off-Minor-V)
    (On-Off-Twice-Minor-V -> On-Off-Minor-V On-Off-Minor-V)
    (On-Off-Thrice-Minor-V
     ->
     On-Off-Twice-Minor-V
     On-Off-Minor-V)
    (On-Off-On-Minor-V -> On-Off-Minor-V GenMinor)
    (On-Off-On-Twice-Minor-V -> On-Off-Twice-Minor-V GenMinor)
    (On-Off-On-To-bVII -> Major-On Bb7 Major-On)
    (On-Off-Twice-On-To-bVII -> On-Off-On-To-bVII Bb7 Major-On)
    (On-Off-Thrice-On-To-bVII
     ->
     On-Off-Twice-On-To-bVII
     Bb7
     Major-On)
    (On-Off-Four-Times-On-To-bVII
     ->
     On-Off-Thrice-On-To-bVII
     Bb7
     Major-On)
    (Diatonic-I-ii-iii -> Major-On Minor-On Minor-On)
    (Diatonic-I-ii-iii-vi -> GM7 Am7 Bm7 Em7)
    (On-Off-to-bVII -> Major-On MajorOrDom)
    (On-Off-Twice-to-bVII -> On-Off-to-bVII On-Off-to-bVII)
    (To-IV-n-Back -> Major-On SuperGenDom IV-n-Back)
    (To-IV-n-Back_blues -> F7_ IV-n-Back_blues)
    (IV-n-Back -> MajorOrDom F$o C)
    (IV-n-Back_blues -> Bb7 Bo7 F7_)
    (Diatonic-Walk-Down -> Em7 Dm7 C)
    (Minor-ii-to-Minor-v
     ->
     On-Off-Minor-Somewhere
     Off-On-Minor-VII
     Minor-On)
    (To-IV-+-Bird-Approach -> Bb Bb7 Eb Ab7)
    (Daahoud-Cadence -> Abm7 Gb7 F7 EM7 EbM7)
    (Rainy-Pullback-Extended
     ->
     Rainy-Pullback
     Straight-Approach)
    (Rainbow-Overrun -> C E7 F Bb7)
    (Rainbow-Overrun_sub1 -> Minor-On Off-On-Major-VII Minor-On)
    (Major-Off-On -> Major-Off-On_ii-I)
    (Major-Off-On_ii-I -> Fm7b5 Major-On)
    (Major-Off-On_iv-I -> Abm Major-On)
    (Major-Off-On-Off -> Major-Off-On Fm7b5)
    (Diatonic-ii-I -> Minor-On Major-On)
    (Approach-from-iii -> Sad-Cadence Nowhere-Approach)
    (Four-Star-Ending -> Four-Star-Approach C)
    (Four-Star-Ending_var1
     ->
     Major-On
     Approach-from-iii
     Body-&-Soul-Cadence)
    (Major-On-+-Dropback -> C6 Em7b5/Bb A7)
    (Descending-Minor-CESH-Approach -> Gm7 Gm/F$ Gm7/F C7)
    (On-+-Dropback -> Major-On Gen7)
    (Tritone-Sub-Turnaround -> On-+-Dropback Tritone-Approach)
    (Somewhere-Turnaround -> Donna-Lee-Start Straight-Approach)
    (Giant-Step-Turnaround -> Giant-Step-Turnaround_var1)
    (Giant-Step-Turnaround_var1 -> Giant-Step Bb7sus4 Bb7)
    (Giant-Step-Turnaround_var2 -> Giant-Step Bb7sus4 Bb7)
    (Seven-sus4-to-3 -> Bb7sus4 Bb7)))

(define S-rules
  '((S -> GenMinor_var1)
    (S -> GenMinor_var2) (S -> GenDom_var1) (S -> GenDom_var2)
    (S -> GenII_var1) (S -> GenII_var2) (S -> LiberalII_var1)
    (S -> LiberalII_var2) (S -> GenIII_var1) (S -> GenIII_var2)
    (S -> GenIII_var3) (S -> GenVI_var1) (S -> GenVI_var2)
    (S -> GenVI_var3) (S -> Gen7_var1) (S -> Gen7_var2)
    (S -> An-Approach) (S -> Liberal-Approach_var1)
    (S -> Liberal-Approach_var2) (S -> SuperGenDom_var1)
    (S -> SuperGenDom_var2) (S -> SuperGenDom_var3)
    (S -> GenMajorTonic_main) (S -> GenMajorTonic_iii)
    (S -> Off_dominant) (S -> Off_major) (S -> Off_minor)
    (S -> Off_minor7) (S -> Off_minor-major)
    (S -> Off_diminished) (S -> TonicOrDom_major)
    (S -> TonicOrDom_minor) (S -> TonicOrDom_dom)
    (S -> MajorOrDom_major) (S -> MajorOrDom_dom)
    (S -> DomOrMinor_minor) (S -> DomOrMinor_minor7)
    (S -> DomOrMinor_dom) (S -> Ascending-Minor-CESH_3step)
    (S -> Ascending-Minor-CESH_4step)
    (S -> Ascending-Minor-CESH-Approach)
    (S -> Descending-Minor-CESH_3step)
    (S -> Descending-Minor-CESH_4step)
    (S -> Ascending-Major-CESH_3step)
    (S -> Ascending-Major-CESH_4step)
    (S -> Ascending-&-Descending-Minor-CESH_4step)
    (S -> Ascending-&-Descending-Minor-CESH_5step)
    (S -> Ascending-&-Descending-Major-CESH)
    (S -> Seven-Chord-Dropback_main)
    (S -> Seven-Chord-Dropback_minoron)
    (S -> Seven-Chord-Dropback_rainy) (S -> Amen-Cadence)
    (S -> Autumn-Leaves-Opening_main)
    (S -> Autumn-Leaves-Opening_withoverrun)
    (S -> Autumnal-Approach_var1) (S -> Autumnal-Approach_var2)
    (S -> Autumnal-Cadence_main) (S -> Autumnal-Cadence_var1)
    (S -> Autumnal-Cadence_var2) (S -> Bemsha-Turnaround-1)
    (S -> Bemsha-Turnaround-2) (S -> Bemsha-Turnaround-3)
    (S -> Body-&-Soul-Approach_main)
    (S -> Body-&-Soul-Approach_var1)
    (S -> Body-&-Soul-Approach_var2)
    (S -> Body-&-Soul-Cadence_major)
    (S -> Body-&-Soul-Cadence_minor)
    (S -> Chromatic-Dropback_main) (S -> Chromatic-Major-Walkup)
    (S -> Chromatic-Minor-Ascending_3steps)
    (S -> Chromatic-Minor-Ascending_4steps)
    (S -> Chromatic-Minor-Descending_3steps)
    (S -> Chromatic-Minor-Descending_4steps)
    (S -> Chromatically-Descending-Dominants_3steps)
    (S -> Chromatically-Descending-Dominants_4steps)
    (S -> Chromatically-Descending-Dominants_5steps)
    (S -> Chromatically-Descending-Dominants_6steps)
    (S -> Coltrane-Cadence) (S -> Cyclic-Approach_2)
    (S -> Cyclic-Approach_3) (S -> Diatonic-Walkup_3stepsfromii)
    (S -> Diatonic-Walkup_4stepsfromii)
    (S -> Diatonic-I-ii-iii-ii) (S -> Diatonic-I-ii-iii-IV)
    (S -> Diatonic-I-ii-iii-biii)
    (S -> Diatonic-I-ii-iii-IV-V-vi) (S -> Diatonic-I-IV-iii-ii)
    (S -> Diatonic-ii-iii-IV) (S -> Diatonic-ii-iii-IV-V)
    (S -> Diatonic-ii-iii-IV-iii-ii) (S -> Dizzy-Approach_var1)
    (S -> Dizzy-Approach_var2) (S -> Dizzy-Cadence_main)
    (S -> Dizzy-Cadence_var1) (S -> Dizzy-Cadence_var2)
    (S -> Dogleg-Approach_short) (S -> Dogleg-Approach_long)
    (S -> Dogleg-Cadence) (S -> Dogleg-Cycle_3steps)
    (S -> Dogleg-Cycle_4steps) (S -> Dogleg-Cycle_5steps)
    (S -> Dogleg-Cycle_6steps) (S -> Dominant-Cycle-2-Steps)
    (S -> Dominant-Cycle-3-Steps) (S -> Dominant-Cycle-4-Steps)
    (S -> Dominant-Cycle-5-Steps) (S -> Dominant-Cycle-6-Steps)
    (S -> Dominant-Cycle-7-Steps) (S -> Dominant-Cycle-8-Steps)
    (S -> Dominant-Cycle_step-2) (S -> Dominant-Cycle_step-3)
    (S -> Dominant-Cycle_step-4) (S -> Dominant-Cycle_step-5)
    (S -> Dominant-Cycle_step-6) (S -> Dominant-Cycle_step-7)
    (S -> Dominant-Cycle_step-8) (S -> Dominant-Cycle-Cadence)
    (S -> Dominant-Turnaround) (S -> Donna-Lee-Start_var1)
    (S -> Donna-Lee-Start_var2) (S -> Donna-Lee-Opening_main)
    (S -> Donna-Lee-Opening_Dizzycadenceending) (S -> Doo-Wop)
    (S -> Double-Pullback) (S -> Dropback_main)
    (S -> Dropback_iii-VI-var) (S -> Dropback_tritone)
    (S -> Dropback_chromatic) (S -> Dropback_TINGLe)
    (S -> Dropback-Approach_main) (S -> Extended-Approach_var1)
    (S -> Extended-Cadence) (S -> Foggy-Cadence)
    (S -> Foggy-Turnaround_main) (S -> Foggy-Turnaround_var)
    (S -> Foolish-Approach) (S -> Four-Star-Approach)
    (S -> GDS-Cadence) (S -> Giant-Step) (S -> Giant-Steps)
    (S -> Giant-Step-Approach) (S -> Happenstance-Cadence)
    (S -> Honeysuckle-Bridge_main)
    (S -> Honeysuckle-Bridge_two-goes)
    (S -> Honeysuckle-Bridge_two-goes-variant)
    (S -> Honeysuckle-Bridge_dogleg)
    (S -> Honeysuckle-Bridge_two-goes-dogleg)
    (S -> Honeysuckle-Bridge_pullback)
    (S -> Honeysuckle-Bridge_supertensionend)
    (S -> II-n-Back_main) (S -> II-n-Back_var)
    (S -> II-n-Bird-Approach) (S -> II-n-Bird-POT)
    (S -> II-n-Bird-SPOT_main) (S -> II-n-Bird-SPOT_var1)
    (S -> To-IV_main) (S -> IV-n-Yak) (S -> To-IV-n-Yak)
    (S -> To-IV-n-Yak-Turnaround) (S -> IV-n-Yak-To)
    (S -> IV-n-Hack) (S -> To-IV-n-Hack) (S -> IV-n-Mack)
    (S -> To-IV-n-Mack) (S -> To-IV-n-Mack-Turnaround)
    (S -> IV-n-Bird) (S -> To-IV-n-Bird)
    (S -> To-IV-n-Back-SPOT) (S -> IV-n-Bird-SPOT_main)
    (S -> IV-n-Bird-SPOT_rainy) (S -> To-IV-n-Bird-SPOT)
    (S -> IV-n-Bauble) (S -> ITCHY-Opening) (S -> La-Bomba)
    (S -> Ladybird-Turnaround_main) (S -> Ladybird-Cadence)
    (S -> Light-&-Day-Approach) (S -> Lonely-Approach)
    (S -> Lonely-Cadence) (S -> Long-Approach_main)
    (S -> Long-Approach_tritone) (S -> Long-Approach_sus)
    (S -> Long-Cadence_main)
    (S -> Minor-Chromatic-Walkdown_2step)
    (S -> Minor-Chromatic-Walkdown_3step)
    (S -> Minor-Chromatic-Walkdown_4step)
    (S -> Minor-Chromatic-Walkdown-Approach)
    (S -> Minor-Dropback) (S -> Minor-Plagal-Cadence)
    (S -> Minor-Perfect-Cadence) (S -> Moment_quotes-Cadence)
    (S -> Moment_quotes-Approach) (S -> Night-&-Day-Cadence)
    (S -> Nobody_quotes-Cadence) (S -> Nowhere-Approach_main)
    (S -> Nowhere-Approach_slow) (S -> Nowhere-Cadence)
    (S -> Nowhere-Minor-Cadence_main)
    (S -> Nowhere-Minor-Cadence_var1) (S -> Nowhere-Turnaround)
    (S -> Nowhere-Turnaround-Minor) (S -> Nowhere-Turnaround+On)
    (S -> Nowhere-Turnaround-to-Minor-On) (S -> Passacaglia)
    (S -> POT-Spring-sub) (S -> POT+On_cyclic) (S -> POT_main)
    (S -> POT_var1) (S -> POT_var2) (S -> POT_var3)
    (S -> POT_var4) (S -> POT_var5) (S -> POT_blues)
    (S -> POT_cyclic) (S -> POT_Yardbird) (S -> POT_sadapproach)
    (S -> POT) (S -> Minor-POT_main) (S -> Minor-POT_var1)
    (S -> Minor-POT_var2) (S -> Minor-POT_var3)
    (S -> Minor-POT_dominant) (S -> Minor-POT_sad)
    (S -> Minor-POT_altereddominant) (S -> Minor-POT+On)
    (S -> Pennies-Approach_main) (S -> Pennies-Approach_var1)
    (S -> Pennies-Approach_var2) (S -> Pennies-Ending_main)
    (S -> Pennies-Ending_raindrop)
    (S -> Pennies-Ending_side-slipping)
    (S -> Pennies-Ending_dropback)
    (S -> Pennies-Ending_Somewhere)
    (S -> Pennies-Ending_Yardbird)
    (S -> Pennies-Turnaround_main)
    (S -> Pennies-Turnaround_TTFA)
    (S -> Pennies-Turnaround-Two-Goes) (S -> Perfect-Cadence)
    (S -> Pullback_basic) (S -> Pullback_var1)
    (S -> Pullback_tritone) (S -> Rainy-Pullback)
    (S -> Pullback-Extended_main)
    (S -> Pullback-Extended_yardbird) (S -> Pullback-to-Cadence)
    (S -> Pullback-Cadence-with-Dropback_rainy)
    (S -> Rainbow-Cadence_var1) (S -> Rainbow-Cadence_var2)
    (S -> Rainbow-Cadence_var3) (S -> Rainbow-Cadence_var4)
    (S -> Raindrop_main) (S -> Raindrop_var1)
    (S -> Raindrop_var2) (S -> Rainy-Turnaround)
    (S -> Rainy-Cadence) (S -> Rainy-Cadence_minorchromatic)
    (S -> Reverse-Dominant-Cycle-2-Steps)
    (S -> Reverse-Dominant-Cycle-3-Steps)
    (S -> Reverse-Dominant-Cycle-4-Steps)
    (S -> Reverse-Dominant-Cycle-5-Steps)
    (S -> Reverse-Dominant-Cycle-6-Steps) (S -> Rhythm-Bridge)
    (S -> Rhythm-Turnaround_main) (S -> Rhythm-Turnaround_var)
    (S -> Rhythm-Turnaround+On) (S -> Sad-Approach)
    (S -> Sad-Cadence_main) (S -> Sad-Cadence_var)
    (S -> Sad-Cadence-with-Overrun_var) (S -> Sad-Dropback)
    (S -> Sad-SPOT) (S -> Satin-Cadence) (S -> Side-Slip)
    (S -> Side-Slips_2) (S -> Side-Slips_2variant)
    (S -> Side-Slips_3) (S -> Side-Slips_4) (S -> Side-Slips_5)
    (S -> Side-Slips_6) (S -> Sharp-Fourpenny-Approach)
    (S -> Sharp-Fourpenny-Ending_main)
    (S -> Sharp-Fourpenny-Ending_var) (S -> Sixpenny-Approach)
    (S -> Sixpenny-Ending) (S -> Spring-Approach_simple)
    (S -> Spring-Approach_extended) (S -> Spring-Cadence_main)
    (S -> Somewhere/Nowhere-Approach) (S -> SPOT_main)
    (S -> SPOT_minorIV) (S -> SPOT_var1) (S -> SPOT_var2)
    (S -> SPOT_var3) (S -> SPOT_sideslip) (S -> Multi-Sub-POT)
    (S -> Stablemates-Approach) (S -> Stablemates-Cadence)
    (S -> Starlight-Approach_main)
    (S -> Starlight-Approach_Dizzy)
    (S -> Starlight-Approach_Dizzyvar2)
    (S -> Starlight-Approach_Night-&-Day)
    (S -> Starlight-Approach_rainy)
    (S -> Starlight-Approach_tritonestart)
    (S -> Starlight-Cadence_main)
    (S -> Starlight-Cadence_airegin)
    (S -> Starlight-Cadence_tension)
    (S -> Starlight-Cadence_Night-&-Day)
    (S -> Starlight-Dropback) (S -> Starlight-Opening)
    (S -> Straight-Approach_main) (S -> Straight-Approach_var)
    (S -> Straight-Cadence_main)
    (S -> Straight-Cadence-with-Dropback)
    (S -> Straight-Cadence-+-Dominant-Overrun)
    (S -> Surge_major) (S -> Surge_minor) (S -> Surge_dominant)
    (S -> Surge_majorvar) (S -> Surge_minorvar)
    (S -> Surge_dominantvar) (S -> Supertension-Ending)
    (S -> Surprise-Minor-Cadence) (S -> Surprise-Major-Cadence)
    (S -> Sus-Approach) (S -> Sus-Approach_var1)
    (S -> Sus-Cadence) (S -> Sus-Cadence_main)
    (S -> Sus-Cadence_var1) (S -> Sus-Cadence_var2)
    (S -> Tension-Cadence) (S -> Tension-Pullback)
    (S -> Tension-SPOT) (S -> To-II-n-Back) (S -> To-Somewhere)
    (S -> Cadence_perfect) (S -> Cadence_straight)
    (S -> Cadence_sad) (S -> Cadence_tritone)
    (S -> Cadence_surge) (S -> To-IV-n-Bird-POT)
    (S -> TTFA-Dropback_main) (S -> TTFA-Dropback_dropback)
    (S -> TTFA-Dropback_IV-variant)
    (S -> TTFA-Dropback_ii-variant)
    (S -> TTFA-Dropback_IInBackvariant)
    (S -> TTFA-Dropback_rainy)
    (S -> TTFA-Dropback_minorchromaticdescent)
    (S -> Twopenny-Approach) (S -> Twopenny-Ending_main)
    (S -> Twopenny-Ending_Nowheredropback)
    (S -> Twopenny-Ending_Starlightback)
    (S -> Twopenny-Ending_var1) (S -> Twopenny-Ending_var2)
    (S -> Twopenny-Ending_var3) (S -> Upslide)
    (S -> Tritone-Approach) (S -> Tritone-Cadence_main)
    (S -> Tritone-Cadence_short)
    (S -> Tritone/Straight-Approach) (S -> Two-Goes-Approach)
    (S -> Two-Goes-Nowhere-Approach)
    (S -> Two-Goes-Nowhere-Cadence) (S -> Two-Goes-Pullback)
    (S -> Two-Goes-Pullback-Extended)
    (S -> Two-Goes-Pullback+Cadence)
    (S -> Two-Goes-Rainy-Turnaround)
    (S -> Two-Goes-Sad-Approach) (S -> Two-Goes-Sad-Cadence)
    (S -> Two-Goes-Starlight-Approach)
    (S -> Two-Goes-Straight-Approach)
    (S -> Two-Goes-Straight-Cadence)
    (S -> Two-Goes-Straight-Cadence-with-Overrun_var1)
    (S -> Two-Goes-Straight-Cadence-with-Overrun_var2)
    (S -> Three-Goes-Pullback)
    (S -> Three-Goes-Pullback-Extended)
    (S -> Three-Goes-Pullback+Cadence)
    (S -> Three-Goes-Sad-Approach) (S -> Three-Goes-Sad-Cadence)
    (S -> Three-Goes-Straight-Approach)
    (S -> Three-Goes-Straight-Cadence)
    (S -> Three-Goes-Nowhere-Approach)
    (S -> Three-Goes-Nowhere-Cadence)
    (S -> Four-Goes-Straight-Approach)
    (S -> Four-Goes-Straight-Cadence)
    (S -> Four-Goes-Sad-Approach) (S -> Four-Goes-Sad-Cadence)
    (S -> Two-Goes-Tritone-Approach)
    (S -> Two-Goes-Tritone-Cadence)
    (S -> Three-Goes-Tritone-Approach)
    (S -> Three-Goes-Tritone-Cadence)
    (S -> Four-Goes-Tritone-Approach)
    (S -> Four-Goes-Tritone-Cadence)
    (S -> Two-Goes-Dominant-Approach)
    (S -> Two-Goes-Tritone-Straight-Approach)
    (S -> Two-Goes-Tritone-Straight-Cadence)
    (S -> Whoopee-Turnaround) (S -> Whoopee-Cadence)
    (S -> Wonderful-Opening) (S -> Yardbird-Approach)
    (S -> Yardbird-Cadence_main) (S -> Yardbird-Cadence_var)
    (S -> Yardbird-Sub-Cadence) (S -> Yardbird-Sub-Turnaround)
    (S -> Major-On) (S -> Minor-On_main) (S -> Minor-On_var1)
    (S -> Minor-On_var2) (S -> Minor-On_var3)
    (S -> On-Off-Major-V) (S -> On-Off-Major-bII)
    (S -> On-Off-Major-Somewhere) (S -> On-Off-Major-bIII)
    (S -> On-Off-Major-III) (S -> On-Off-Major-IV)
    (S -> On-Off-Major-$IV) (S -> On-Off-Major-Nowhere)
    (S -> On-Off-Major-VI) (S -> On-Off-Major-bVII)
    (S -> On-Off-Major-VII) (S -> On-Off-Minor-bII)
    (S -> On-Off-Minor-Somewhere) (S -> On-Off-Minor-bIII)
    (S -> On-Off-Minor-III) (S -> On-Off-Minor-IV)
    (S -> On-Off-Minor-$IV) (S -> On-Off-Minor-V)
    (S -> On-Off-Minor-Nowhere) (S -> On-Off-Minor-VI)
    (S -> On-Off-Minor-bVII) (S -> On-Off-Minor-VII)
    (S -> Off-On-Major-bII) (S -> Off-On-Major-Somewhere)
    (S -> Off-On-Major-bIII) (S -> Off-On-Major-III)
    (S -> Off-On-Major-IV) (S -> Off-On-Major-$IV)
    (S -> Off-On-Major-Nowhere) (S -> Off-On-Major-VI)
    (S -> Off-On-Major-bVII) (S -> Off-On-Major-VII)
    (S -> Off-On-Minor-bII) (S -> Off-On-Minor-Somewhere)
    (S -> Off-On-Minor-bIII) (S -> Off-On-Minor-III)
    (S -> Off-On-Minor-IV) (S -> Off-On-Minor-$IV)
    (S -> Off-On-Minor-Nowhere) (S -> Off-On-Minor-VI)
    (S -> Off-On-Minor-bVII) (S -> Off-On-Minor-VII)
    (S -> On-Off-Twice-To-V) (S -> On-Off-On-To-IV-Major)
    (S -> On-Off-On-+Dropback-IV-Major)
    (S -> On-Off-On-To-IV-Minor) (S -> On-Off-Twice-On-To-IV)
    (S -> On-Off-Thrice-To-IV) (S -> On-Off-Thrice-On-To-IV)
    (S -> On-Off-Four-Times-On-To-IV) (S -> On-Off-On-To-V_main)
    (S -> On-Off-On-To-V_minor) (S -> On-Off-On-To-V_sus)
    (S -> On-Off-Twice-On-To-V) (S -> On-Off-Thrice-On-To-V)
    (S -> On-Off-Four-Times-On-To-V) (S -> On-Off-On-To-II)
    (S -> On-Off-Twice-To-II) (S -> On-Off-Twice-On-To-II)
    (S -> On-Off-Thrice-On-To-II)
    (S -> On-Off-Four-Times-On-To-II)
    (S -> On-Off-To-IV-Twice-Major-then-Minor)
    (S -> On-Off-Major-Minor-V) (S -> On-Off-Twice-Minor-V)
    (S -> On-Off-Thrice-Minor-V) (S -> On-Off-On-Minor-V)
    (S -> On-Off-On-Twice-Minor-V) (S -> On-Off-On-To-bVII)
    (S -> On-Off-Twice-On-To-bVII)
    (S -> On-Off-Thrice-On-To-bVII)
    (S -> On-Off-Four-Times-On-To-bVII) (S -> Diatonic-I-ii-iii)
    (S -> Diatonic-I-ii-iii-vi) (S -> On-Off-to-bVII)
    (S -> On-Off-Twice-to-bVII) (S -> To-IV-n-Back)
    (S -> To-IV-n-Back_blues) (S -> IV-n-Back)
    (S -> IV-n-Back_blues) (S -> Diatonic-Walk-Down)
    (S -> Minor-ii-to-Minor-v) (S -> To-IV-+-Bird-Approach)
    (S -> Daahoud-Cadence) (S -> Rainy-Pullback-Extended)
    (S -> Rainbow-Overrun) (S -> Rainbow-Overrun_sub1)
    (S -> Major-Off-On_ii-I) (S -> Major-Off-On_iv-I)
    (S -> Major-Off-On-Off) (S -> Diatonic-ii-I)
    (S -> Approach-from-iii) (S -> Four-Star-Ending)
    (S -> Four-Star-Ending_var1) (S -> Major-On-+-Dropback)
    (S -> Descending-Minor-CESH-Approach) (S -> On-+-Dropback)
    (S -> Tritone-Sub-Turnaround) (S -> Somewhere-Turnaround)
    (S -> Giant-Step-Turnaround_var1)
    (S -> Giant-Step-Turnaround_var2) (S -> Seven-sus4-to-3)))
