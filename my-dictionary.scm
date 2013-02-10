;;; Modified Scheme-compatible Impro-Visor bricks dictionary.

;;; Adapted from My.dictionary, version 1.4, Sat. Dec. 3, 2011
;;; Copyright (C) 2011 Robert Keller and Harvey Mudd College

;;; This dictionary was compiled by Zack Merritt, Xanda Schofield, and
;;; Robert Keller, June-Sept. 2011.

(load "prelude.scm")
(load "pmatch.scm")

(define brick?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'brick-tag))))

(define-syntax defbrick
  (lambda (x)
    (syntax-case x ()
      [(_ name (qualifier* ...) mode type key
          subblock
          subblock*
          ...)
       (with-syntax ((qn (datum->syntax
                          (syntax name)
                          (qualified-name
                           (syntax->datum (syntax name))
                           (syntax->datum (syntax (qualifier* ...)))))))
         (syntax
          (define qn
            (let ([qualifier '(qualifier* ...)])
              `(brick-tag
                qn
                (name ,qualifier)
                mode
                type
                key
                (subblock
                 subblock*
                 ...))))))]
      [(_ name mode type key
          subblock
          subblock*
          ...)
       (syntax
        (defbrick name () mode type key
          subblock
          subblock*
          ...))])))

(define-syntax def-default-brick
  (syntax-rules ()
    [(_ default-name brick)
     (define default-name
       (pmatch brick
         [(brick-tag ,qn (,name ,qualifier) ,mode ,type ,key ,subblocks)
          `(brick-tag
            default-name
            (default-name ,qualifier)
            ,mode
            ,type
            ,key
            ,subblocks)]))]))

;;; Invisible bricks
;;; These are bricks that are used in constructing other bricks.
;;; They do not appear explicitly in the user's dictionary.

;;; GenMinor is a generalized minor tonic

(defbrick GenMinor(var 1) Minor Invisible C
         (chord Cm 1))

(defbrick GenMinor(var 2) Minor Invisible C
          (chord Cm7 1))

;;; GenDom is a generalized dominant, either a V or a ii-V

(defbrick GenDom(var 1) Major Invisible C
          (chord G7 1))

(defbrick GenDom(var 2) Major Invisible C
          (brick GenII C 1)
          (chord G7 1))

;;; GenII is a Generalized II chord, either a m7 or m7b5

(defbrick GenII(var 1) Major Invisible C
          (chord Dm7 1))

(defbrick GenII(var 2) Major Invisible C
          (chord Dm7b5 1))

;;; LiberalII is a Liberal II chord, either a m7 or m7b5 or a 7

(defbrick LiberalII(var 1) Major Invisible C
          (brick GenII C 1))

(defbrick LiberalII(var 2) Major Invisible C
          (chord D7 1))

;;; GenIII is a Generalized III chord, either a 7, m7 or m7b5

(defbrick GenIII(var 1) Major Invisible C
          (chord Em7 1))

(defbrick GenIII(var 2) Major Invisible C
          (chord E7 1))

(defbrick GenIII(var 3) Major Invisible C
          (chord Em7b5 1))

;;; GenVI is a Generalized VI chord, either a 7, m7 or m7b5

(defbrick GenVI(var 1) Major Invisible C
          (chord Am7 1))

(defbrick GenVI(var 2) Major Invisible C
          (chord A7 1))

(defbrick GenVI(var 3) Major Invisible C
          (chord Am7b5 1))

;;; Gen7 is a 7 or m7, as used in a dropback or turnaround, for example

(defbrick Gen7(var 1) Dominant Invisible C
         (chord C7 1))

(defbrick Gen7(var 2) Dominant Invisible C
          (chord Cm7 1))

;;; An-Approach is a GenII followed by the corresponding dominant
;;; so it serves as a general II-V progression.

(defbrick An-Approach Major Invisible C
          (brick GenII C 1)
          (chord G7 1))

;;; Liberal-Approach is even more liberal than An-Approach
;;; It allows a minor seventh to substitute for a dominant

(defbrick Liberal-Approach(var 1) Major Invisible C
          (brick An-Approach C 1))

(defbrick Liberal-Approach(var 2) Major Invisible C
          (brick GenII C 1)
          (chord Gm7 1))

;;; SuperGenDom is a V, or a GenII followed by a V, 
;;; or a tritone sub for the latter

(defbrick SuperGenDom(var 1) Major Invisible C
          (chord G7 1))

(defbrick SuperGenDom(var 2) Major Invisible C
          (brick GenII C 1)
          (chord G7 1))

(defbrick SuperGenDom(var 3) Major Invisible C
          (brick GenII C 1)
          (chord Db7 1))

;;; GenMajorTonic is either a Major I chord 
;;; or a Major iii chord, which can sub for I

(defbrick GenMajorTonic(main) Major Invisible C
          (chord C 1))

(defbrick GenMajorTonic(iii) Major Invisible C
          (chord Em7 1))

;;; Varieties of "Off", chords that contrast a I chord

(defbrick Off(dominant) Dominant Invisible C
          (chord C7 1))

(defbrick Off(major) Major Invisible C
          (chord C 1))

(defbrick Off(minor) Minor Invisible C
          (chord Cm 1))

(defbrick Off(minor7) Minor Invisible C
          (chord Cm7 1))

(defbrick Off(minor-major) Minor Invisible C
          (chord CmM7 1))

(defbrick Off(diminished) Minor Invisible C
          (chord Co7 1))

;;; TonicOrDom is a chord that is either major, minor, or dominant on a given root

(defbrick TonicOrDom(major) Major Invisible C
          (chord C 1))

(defbrick TonicOrDom(minor) Minor Invisible C
          (chord Cm 1))

(defbrick TonicOrDom(dom) Dominant Invisible C
          (chord C7 1))

;;; MajorOrDom is a chord that is either major or dominant on a given root

(defbrick MajorOrDom(major) Major Invisible C
          (chord C 1))

(defbrick MajorOrDom(dom) Dominant Invisible C
          (chord C7 1))

;;; DomOrMinor is a minor, minor 7, or dominant on a given root on degree V of the key.

(defbrick DomOrMinor(minor) Dominant Invisible C
          (chord Gm 1))

(defbrick DomOrMinor(minor7) Dominant Invisible C
          (chord Gm7 1))

(defbrick DomOrMinor(dom) Dominant Invisible C
          (chord G7 1))

;;; End of Invisible Bricks

;;; CESH = Contrapunal Elaboration of Static Harmony

(defbrick Ascending-Minor-CESH(3 step) Minor CESH C
          (chord Cm 1)
          (chord Cm+ 1)
          (chord Cm6 1))

(defbrick Ascending-Minor-CESH(4 step) Minor CESH C
          (chord Cm 1)
          (chord Cm+ 1)
          (chord Cm6 1)
          (chord Cm7 1))

(defbrick Ascending-Minor-CESH-Approach Major Approach F
          (chord Cm 1)
          (chord Cm+ 1)
          (chord Cm6 1)
          (chord C7 1))

(defbrick Descending-Minor-CESH(3 step) Minor CESH C
          (chord Cm 1)
          (chord CmM7 1)
          (chord Cm7 1))

(defbrick Descending-Minor-CESH(4 step) Minor CESH C
          (brick Descending-Minor-CESH(3 step) C 3)
          (chord Cm6 1))

(defbrick Ascending-Major-CESH(3 step) Major CESH C
          (chord C 1)
          (chord C+ 1)
          (chord C6 1))

(defbrick Ascending-Major-CESH(4 step) Major CESH C
          (chord C 1)
          (chord C+ 1)
          (chord C6 1)
          (chord C7 1))

(defbrick Ascending-&-Descending-Minor-CESH(4 step) Minor CESH C
          (chord Cm 1)
          (chord Cm+ 1)
          (chord Cm6 1)
          (chord Cm+ 1))

(defbrick Ascending-&-Descending-Minor-CESH(5 step) Minor CESH C
          (chord Cm 1)
          (chord Cm+ 1)
          (chord Cm6 1)
          (chord Cm7 1)
          (chord Cm6 1))

(defbrick Ascending-&-Descending-Major-CESH Major CESH Ab
        (brick Major-On Ab 1)
        (chord AbM7$5 1)
        (brick Major-On Ab 1)
        (chord AbM7$5 1)
        (chord AbM7 1))

;;; End of CESH

(defbrick Seven-Chord-Dropback(main) Major Cadence C
          (brick Dropback-Approach C 6)
          (chord C 1))

(defbrick Seven-Chord-Dropback(minor on) Major Cadence F
          (brick Minor-On Bb 1)
          (brick Perfect-Cadence F 2)
          (brick Long-Cadence F 5))

(defbrick Seven-Chord-Dropback(rainy) Major Cadence C
          (chord Dm7 1)
          (chord G7 1)
          (chord C 1)
          (chord Ebo 1)
          (chord Dm7 1)
          (chord G7 1)
          (chord C 1))  

(defbrick Amen-Cadence Major Cadence C
          (brick TonicOrDom F 1)
          (chord C 1))

(defbrick Autumn-Leaves-Opening(main) Major Opening C 
          (brick Straight-Approach C 1)
          (chord C 2)
          (brick Sad-Cadence A 4))

(defbrick Autumn-Leaves-Opening(with overrun) Major Opening C
          (brick Straight-Approach C 1)
          (chord C 1)
          (chord F 1)
          (brick Sad-Cadence A 4))

;;; Autumnal stuff

(defbrick Autumnal-Approach(var 1) Minor Approach A
          (brick An-Approach C 2)
          (brick An-Approach A 2))

(defbrick Autumnal-Approach(var 2) Minor Approach A
          (brick Minor-On D 2)
          (brick An-Approach A 2))

(defbrick Autumnal-Cadence(main) Minor Cadence C
          (brick Autumnal-Approach C 3)
          (brick GenMinor C 1))

(defbrick Autumnal-Cadence(var 1) Minor Cadence C
          (brick GenII Eb 1)
          (brick Sad-Cadence C 2))

(defbrick Autumnal-Cadence(var 2) Minor Cadence C
          (brick GenMinor F 1) 
          (brick Cadence C 3))

;;; End of Autumnal stuff

;;; Bemsha are several variations on turnarounds with tritone substitutions,
;;; as occuring in Monk's "Bemsha Swing"

(defbrick Bemsha-Turnaround-1 Major Turnaround C
          (chord C 1)
          (chord Eb7 1)
          (chord D7 1)
          (chord Db7 1))

(defbrick Bemsha-Turnaround-2 Major Turnaround C
          (chord C 1)
          (chord Ab7 1)
          (chord DbM7 1)
          (chord Gb7 1))

(defbrick Bemsha-Turnaround-3 Major Turnaround C
          (chord C 1)
          (chord Bb7 1)
          (chord EbM7 1)
          (chord Ab7 1))

;;; Body-and-Soul Stuff

(defbrick Body-&-Soul-Approach(main) Major Approach C
          (chord Dm7 1)
          (brick GenDom D 1)
          (brick An-Approach C 1))

(defbrick Body-&-Soul-Approach(var 1) Major Approach C
          (chord Dm 1)
          (chord A7 1)
          (chord Ab7 1)
          (brick GenDom C 1)) ;;; modified by XS

(defbrick Body-&-Soul-Approach(var 2) Major Approach C
          (brick On-Off-Minor-Somewhere D 2)
          (brick Straight-Approach C 2))


(defbrick Body-&-Soul-Cadence(major) Major Cadence C
          (brick Body-&-Soul-Approach C 4)
          (chord C 4))

(defbrick Body-&-Soul-Cadence(minor) Minor Cadence C
          (brick Body-&-Soul-Approach C 4)
          (chord Cm 4))

;;; End of Body-and-Soul Stuff

;;; Chromatic stuff

(defbrick Chromatic-Dropback(main) Major Dropback C
          (chord C 1)
          (chord B7 1)
          (chord Bb7 1)
          (chord A7 1))

(defbrick Chromatic-Major-Walkup Major Cadence C
          (brick Major-On Bb 1)
          (brick On-Off-Major-bII B 3))

(defbrick Chromatic-Minor-Ascending(3 steps) Minor Approach C
          (brick GenII Ab 1)
          (brick GenII A  1)
          (brick GenII Bb  1))

(defbrick Chromatic-Minor-Ascending(4 steps) Minor Approach C
          (brick Chromatic-Minor-Ascending(3 steps) B 3)
          (brick GenII Bb 1))

(defbrick Chromatic-Minor-Descending(3 steps) Minor Approach C
          (chord Dm7 1)
          (chord Dbm7 1)
          (chord Cm7 1))

(defbrick Chromatic-Minor-Descending(4 steps) Minor Approach C
          (brick Chromatic-Minor-Descending(3 steps) C$ 3)
          (chord Cm7 1))

(defbrick Chromatically-Descending-Dominants(3 steps) Major Approach C
          (chord A7 1)
          (chord Ab7 1)
          (chord G7 1))

(defbrick Chromatically-Descending-Dominants(4 steps) Major Approach C
          (chord Bb7 1)
          (brick Chromatically-Descending-Dominants(3 steps) C 3))

(defbrick Chromatically-Descending-Dominants(5 steps) Major Approach C
          (chord B7 1)
          (brick Chromatically-Descending-Dominants(4 steps) C 4))

(defbrick Chromatically-Descending-Dominants(6 steps) Major Approach C
          (chord C7 1)
          (brick Chromatically-Descending-Dominants(5 steps) C 5))

;;; End of Chromatic Stuff

;;; Coltrane

(defbrick Coltrane-Cadence Major Cadence C
          (chord Dm7 1) 
          (brick Perfect-Cadence Ab 2) 
          (brick Perfect-Cadence E 2) 
          (brick Perfect-Cadence C 2))

;;; Cyclic

(defbrick Cyclic-Approach(2) Major Approach C
          (chord Dm7 1)
          (chord D7 1)
          (chord Gm7 1)
          (chord G7 1))

(defbrick Cyclic-Approach(3) Major Approach C
          (brick Cyclic-Approach(2) G 4)
          (chord Gm7 1)
          (chord G7 1))

;;; Diatonic stuff

(defbrick Diatonic-Walkup(3 steps from ii) Minor Approach F
          (chord Dm7 1)
          (chord Em7 1)
          (chord Fm7 1))

(defbrick Diatonic-Walkup(4 steps from ii) Minor Approach F
          (chord Dm7 1)
          (chord Em7 1)
          (chord Fm7 1)
          (chord Gb7 1))

(defbrick Diatonic-I-ii-iii-ii Major Approach C
          (chord C 1)
          (chord Dm7 1)
          (chord Em7 1)
          (chord Dm7 1))

(defbrick Diatonic-I-ii-iii-IV Major Cadence C
          (chord C 1)
          (chord Dm7 1)
          (chord Em7 1)
          (chord F 1))

(defbrick Diatonic-I-ii-iii-biii Major Approach C
          (chord C 1)
          (chord Dm7 1)
          (chord Em7 1)
          (chord Ebm7 1))

(defbrick Diatonic-I-ii-iii-IV-V-vi Major Approach C
          (chord C 1)
          (chord Dm7 1)
          (chord Em7 1)
          (chord F 1)
          (chord G7 1)
          (chord Am7 1))

(defbrick Diatonic-I-IV-iii-ii Major Approach C
          (chord C 1)
          (chord F 1)
          (chord Em7 1)
          (chord Dm7 1))

(defbrick Diatonic-ii-iii-IV Major Cadence C
          (chord Dm7 1)
          (chord Em7 1)
          (chord F 1))

(defbrick Diatonic-ii-iii-IV-V Major Cadence C
          (brick Diatonic-ii-iii-IV C 3)
          (chord G7 1))

(defbrick Diatonic-ii-iii-IV-iii-ii Major Approach C
         (chord Dm7 1)
          (chord Em7 1)
          (chord F 1)
          (chord Em7 1)
          (chord Dm7 1))

;;; End of Diatonic stuff


;;; Dizzy stuff

(defbrick Dizzy-Approach(var 1) Major Approach C
          (chord Dm 2)
          (brick Straight-Approach Gb 2))

(defbrick Dizzy-Approach(var 2) Minor Approach C
          (chord Dm7b5 1)
          (chord Db7 1))

(defbrick Dizzy-Cadence(main) Major Cadence C
          (brick Dizzy-Approach C 4)
          (chord C 4))

(defbrick Dizzy-Cadence(var 1) Major Cadence C
          (chord G7 2)
          (brick Tritone-Cadence C 2))

(defbrick Dizzy-Cadence(var 2) Major Cadence C
          (brick Nowhere-Approach F$ 2)
          (chord C 1))

;;; End of Dizzy stuff


;;; Dogleg stuff

(defbrick Dogleg-Approach(short) Major Approach C
          (chord D7 1)
          (chord Dm7 1)
          (chord G7 1))

(defbrick Dogleg-Approach(long) Major Approach C
          (brick Straight-Approach G 2)
          (brick Straight-Approach C 2))

(defbrick Dogleg-Cadence Major Cadence C
          (brick Dogleg-Approach C 3)
          (chord C 1))

(defbrick Dogleg-Cycle(3 steps) Major Approach C
          (brick Straight-Approach D 1)
          (brick Straight-Approach G 1)
          (brick Straight-Approach C 1))

(defbrick Dogleg-Cycle(4 steps) Major Approach C
          (brick Straight-Approach A 1)
          (brick Dogleg-Cycle(3 steps) C 3))

(defbrick Dogleg-Cycle(5 steps) Major Approach C
          (brick Straight-Approach E 1)
          (brick Dogleg-Cycle(4 steps) C 4))

(defbrick Dogleg-Cycle(6 steps) Major Approach C
          (brick Straight-Approach B 1)
          (brick Dogleg-Cycle(5 steps) C 5))

;;; End of Dogleg stuff


(defbrick Dominant-Cycle-2-Steps Major Approach C
          (chord D7 1)
          (chord G7 1))

(defbrick Dominant-Cycle-3-Steps Major Approach C
          (chord A7 1)
          (brick Dominant-Cycle-2-Steps C 2))

(defbrick Dominant-Cycle-4-Steps Major Approach C
          (chord E7 1)
          (brick Dominant-Cycle-3-Steps C 3))

(defbrick  Dominant-Cycle-5-Steps Major Approach C
          (chord B7 1)
          (brick Dominant-Cycle-4-Steps C 4))

(defbrick Dominant-Cycle-6-Steps Major Approach C
          (chord F$7 1)
          (brick Dominant-Cycle-5-Steps C 5))

(defbrick Dominant-Cycle-7-Steps Major Approach C
          (chord Db7 1)
          (brick Dominant-Cycle-6-Steps C 5))

(defbrick Dominant-Cycle-8-Steps Major Approach C
          (chord Ab7 1)
          (brick Dominant-Cycle-7-Steps C 5))

(defbrick Dominant-Cycle(Two-step) Major Approach C
           (brick Dominant-Cycle-2-Steps C 1))

(defbrick Dominant-Cycle(Three-step) Major Approach C
           (brick Dominant-Cycle-3-Steps C 1))

(defbrick Dominant-Cycle(Four-step) Major Approach C
           (brick Dominant-Cycle-4-Steps C 1))

(defbrick Dominant-Cycle(Five-step) Major Approach C
           (brick Dominant-Cycle-5-Steps C 1))

(defbrick Dominant-Cycle(Six-step) Major Approach C
           (brick Dominant-Cycle-6-Steps C 1))

(defbrick Dominant-Cycle(Seven-step) Major Approach C
           (brick Dominant-Cycle-7-Steps C 1))

(defbrick Dominant-Cycle(Eight-step) Major Approach C
           (brick Dominant-Cycle-8-Steps C 1))

(defbrick Dominant-Cycle-Cadence Major Cadence C
          (brick Dominant-Cycle C 1)
          (brick Major-On C 1))

(defbrick Dominant-Turnaround Dominant Turnaround C
         (chord C7 1)
         (chord Ab7 1)
         (chord G7 1)
         (chord C7 1))

;;; Donna Lee stuff

(defbrick Donna-Lee-Start(var 1) Major Approach C
        (chord C 2)
        (brick Straight-Approach G 2))

(defbrick Donna-Lee-Start(var 2) Major Approach C
         (chord C 2)
         (brick GenDom G 2))

(defbrick Donna-Lee-Opening(main) Major Opening C
         (brick Donna-Lee-Start C 4)
         (brick Straight-Cadence C 4))

(defbrick Donna-Lee-Opening(Dizzy cadence ending) Major Cadence C
         (brick Donna-Lee-Start C 4)
         (brick Tritone-Cadence C 4))

;;; End of Donna Lee Stuff

;;; Doo-Wop

(defbrick Doo-Wop Major Turnaround C
        (chord C 1)
        (chord Am 1)
        (chord F 1)
        (chord G 1))

;;; Double

;;; Double-Pullback pulls back twice diatonically, not temporally

(defbrick Double-Pullback Major Approach C
          (brick An-Approach C 2)
          (brick An-Approach D 2)
          (brick An-Approach E 2))

;;; Dropbacks

(defbrick Dropback(main) Major Dropback C
          (chord C 2)
          (chord A7 2))

(defbrick Dropback(iii-VI-var) Major Dropback C
          (chord C 2)
          (chord Em7 1)
          (chord A7 1))

(defbrick Dropback(tritone) Major Dropback C
          (chord C 2)
          (chord Bb7 1)
          (chord A7  1))    

(defbrick Dropback(chromatic) Major Dropback C
          (chord C 1)
          (chord B7 1)
          (chord Bb7 1)
          (chord A7 1))

;;; TINGLe = "There is No Greater Love"

(defbrick Dropback(TINGLe) Major Dropback C
          (chord C 1)
          (brick TonicOrDom F 1)
          (brick Nowhere-Approach D 2))

;;; Dropback-Approaches are used in defining "7-chord DropBacks"
;;; which are actually Cadences, not Dropbacks

(defbrick Dropback-Approach(main) Major Invisible C
          (brick Straight-Cadence C 3)
          (brick Extended-Approach C 4))

;;; End of Dropbacks


;;; Extended Approaches and Cadences begin with a VI chord

(defbrick Extended-Approach(var 1) Major Approach C
          (brick GenVI C 1)
          (brick Straight-Approach C 2))

(defbrick Extended-Cadence Major Cadence C
          (brick Extended-Approach C 3)
          (chord C 1))


;;; Foggy stuff

(defbrick Foggy-Cadence Major Cadence C
          (chord Eb7 1)
          (brick Straight-Approach C 2)
          (chord C 1))

(defbrick Foggy-Turnaround(main) Major Turnaround C
          (chord C 1)
          (chord Eb7 1)
          (brick Straight-Approach C 2))

(defbrick Foggy-Turnaround(var) Major Turnaround C
          (chord C 1)
          (chord Eb7 1)
          (chord D7 1)
          (chord G7 1))

;;; Foolish stuff

(defbrick Foolish-Approach Major Approach C
        (brick Raindrop Bb 2)
        (brick Straight-Approach C 2))

;;; Four-Star

(defbrick Four-Star-Approach Major Approach C
          (chord F 1)
          (brick Straight-Approach E 1)
          (brick Long-Approach C 4))

(defbrick Four-Star-Ending Major Ending C
          (brick Four-Star-Approach C 12)
          (chord C 2))

;;; GDS = "Green Dolphin Street"

(defbrick GDS-Cadence Major Cadence C
          (chord D/C 1)
          (chord Db/C 1)
          (chord C 2))

;;; Giant Steps

(defbrick Giant-Step Major Cadence G
        (brick Major-On B 1)
        (brick Perfect-Cadence G 2))

(defbrick Giant-Steps Major Cadence Eb
        (brick Giant-Step G 3)
        (brick Perfect-Cadence Eb 2))

(defbrick Giant-Step-Approach Major Approach A
        (chord Ab7 1)
        (brick Major-On Db 1)
        (chord E7 1))

;;; Happenstance, from "It Could Happen to You"

(defbrick Happenstance-Cadence Major Cadence C
          (brick An-Approach E 2)
          (chord C 2))

;;; Honeysuckle Bridge stuff, from "Honeysuckle Rose"
;;; Both approaches and cadences are present

(defbrick Honeysuckle-Bridge(main) Major Approach C 
          (brick Straight-Cadence F 4) 
          (brick Straight-Approach G 2) 
          (brick Straight-Approach C 2))

(defbrick Honeysuckle-Bridge(two-goes) Major Approach C 
          (brick Two-Goes-Straight-Approach F 2) 
          (chord F 2)
          (brick Two-Goes-Straight-Approach G 2) 
          (chord G7 1))

(defbrick Honeysuckle-Bridge(two-goes-variant) Major Approach C
          (brick Straight-Approach F 2)
          (brick Honeysuckle-Bridge C 7))

(defbrick Honeysuckle-Bridge(dogleg) Major Cadence C
          (brick Straight-Cadence F 5)
          (brick Dogleg-Cycle C 8))

(defbrick Honeysuckle-Bridge(two-goes-dogleg) Major Approach C
          (brick Two-Goes-Straight-Approach F 4)
          (brick Dropback F 6)
          (brick An-Approach C 6))

(defbrick Honeysuckle-Bridge(pullback) Major Approach Eb
          (brick Pullback-to-Cadence Eb 8)
          (brick Pullback F 4)
          (brick Dominant-Cycle-2-Steps Bb 4))

(defbrick Honeysuckle-Bridge(supertension end) Dominant Cadence C
          (brick Straight-Cadence Bb 4)
          (brick Tension-Cadence C 3))

;;; End of Honeysuckle Bridge stuff


;;; II 'n ... stuff (see also "To ...")

(defbrick II-n-Back(main) Major Cadence C
          (chord Dm7 1)
          (chord D$o 1)
          (chord C/E 1))

(defbrick II-n-Back(var) Major Cadence C
          (chord Dm7 1)
          (chord D$o 1)
          (chord Em 1))

(defbrick II-n-Bird-Approach Major Approach F
          (brick Minor-On G 2)
          (brick Yardbird-Approach F 2))

(defbrick II-n-Bird-POT Major Turnaround C
          (chord Dm7 2)
          (brick GenDom Eb 2)
          (brick POT C 2))

(defbrick II-n-Bird-SPOT(main) Major Turnaround C
          (chord Dm7 2)
          (brick GenDom Eb 2)
          (brick SPOT C 2))

(defbrick II-n-Bird-SPOT(var 1) Major Turnaround C
          (brick Minor-On D 2)
          (brick Straight-Approach Eb 2)
          (brick Rainy-Turnaround C 4))

;;; End of II'n stuff


;;; IV 'n ... stuff (see also "To ...")

;;; To get to IV only:

(defbrick To-IV(main) Major Cadence C
          (brick Major-On C 1)
          (brick Cadence  F 2))

;;; To get back from IV:

(defbrick IV-n-Back Major Cadence C
          (brick MajorOrDom F 1)
          (chord F$o          1)
          (chord C            1))

;;; To get to IV and get back:

(defbrick To-IV-n-Back Major Cadence C
          (brick Major-On    C 1)
          (brick SuperGenDom F 2)
          (brick IV-n-Back   C 3))

;;; To get back from IV via Yardbird:

(defbrick IV-n-Yak Major Cadence C
          (brick MajorOrDom  F 1)
          (brick GenDom      Eb 1)
          (brick Major-On    C 1))

;;; To get to IV and get back via Yardbird:

(defbrick To-IV-n-Yak Major Cadence C
          (brick Major-On    C 1)
          (brick SuperGenDom F 2)
          (brick IV-n-Yak    C 3))

(defbrick To-IV-n-Yak-Turnaround Major Turnaround C
          (brick Major-On       C 1)
          (brick SuperGenDom F 2)
          (brick MajorOrDom   F 1)
          (brick GenDom        Eb 1))

;;; To get from IV-n-Yak and on to IV

(defbrick IV-n-Yak-To Major Approach C
        (brick IV-n-Yak C 3)
        (chord C7 1))

;;; To get back from IV via Happenstance:

(defbrick IV-n-Hack Major Cadence C
          (brick TonicOrDom  F 1)
          (brick Happenstance-Cadence C 2))

;;; To get to IV and get back via Hapenstance:

(defbrick To-IV-n-Hack Major Cadence C
          (brick Major-On    C 1)
          (brick SuperGenDom F 2)
          (brick IV-n-Hack    C 3))

;;; To get back from IV via Minor:

(defbrick IV-n-Mack Major Cadence C
          (brick Major-On F 1)
          (brick Minor-Plagal-Cadence C 2))

;;; To get to IV and back via Minor:

(defbrick To-IV-n-Mack Major Cadence C
          (brick To-IV C 3)
          (brick Minor-Plagal-Cadence C 2))

(defbrick To-IV-n-Mack-Turnaround Major Turnaround C
          (brick To-IV C 3)
          (brick Minor-On F 1))

;;; IV-n-Bird goes to iii instead of I

(defbrick IV-n-Bird Major Deceptive-Cadence C
          (brick TonicOrDom  F 1)
          (brick GenDom      Eb 1)
          (brick Minor-On    E 1))

;;; To get to IV and get back via Yardbird:

(defbrick To-IV-n-Bird Major Deceptive-Cadence C
          (brick Major-On    C 1)
          (brick SuperGenDom F 2)
          (brick IV-n-Bird   C 3))

(defbrick To-IV-n-Back-SPOT Major Turnaround F
        (chord FM7 1)
        (chord F7 1)
        (brick Surge Bb 2)
        (brick SPOT F 4))

(defbrick IV-n-Bird-SPOT(main) Major Turnaround C
          (brick TonicOrDom  F  1)
          (brick GenDom      Eb 1)
          (brick SPOT        C  2))          
          
(defbrick IV-n-Bird-SPOT(rainy) Major Turnaround C
          (brick IV-n-Bird C 1)
          (brick Rainy-Turnaround C 4))

(defbrick To-IV-n-Bird-SPOT Major Turnaround C
          (brick Major-On       C 1)
          (brick SuperGenDom    F 1)
          (brick IV-n-Bird-SPOT C 3))

(defbrick IV-n-Bauble Major Approach C
          (brick Upslide F 2)
          (brick Sad-Approach A 2))
  
;;; End of IV'n stuff


;;; ITCHY = "It Could Happen to You" (see also "Happenstance")

(defbrick ITCHY-Opening Major Opening C
          (chord C 2)
          (brick An-Approach D 2)
          (chord Dm 2)
          (brick An-Approach E 2))

;;; La Bomba

(defbrick La-Bomba Major Turnaround C
        (chord C 1)
        (chord F 1)
        (chord G 1)
        (chord F 1))

;;; Ladybird stuff

(defbrick Ladybird-Turnaround(main) Major Turnaround C
          (brick Major-On C 1)
          (brick TonicOrDom Eb 1)
          (brick TonicOrDom Ab 1)
          (brick TonicOrDom Db 1))

(defbrick Ladybird-Cadence Major Cadence C
          (brick Ladybird-Turnaround C 2)
          (chord C 1))

;;; Light & Day, a variant on "Night & Day" and "Starlight"

(defbrick Light-&-Day-Approach Minor Approach C
          (chord Dm7b5 1)
          (chord Dbm 1))

;;; Lonely, from "Alone Together"

(defbrick Lonely-Approach Major Approach C
        (brick Straight-Approach E 2)
        (brick Straight-Approach C 2))

(defbrick Lonely-Cadence Major Cadence C
        (brick Lonely-Approach C 4)
        (chord C 1))

;;; Long stuff: approaches and cadences

(defbrick Long-Approach(main) Major Approach C
          (brick GenIII C 1)
          (brick Extended-Approach C 3))

(defbrick Long-Approach(tritone) Major Approach C
          (brick Dizzy-Approach D 2)
          (chord Abm7 1)
          (chord Db7 1))

(defbrick Long-Approach(sus) Major Approach C
          (brick Surprise-Minor-Cadence D 3)
          (chord F/G 1))


(defbrick Long-Cadence(main) Major Cadence C
          (brick Long-Approach C 4)
          (chord C 1))

;;; End of Long stuff

;;; Minor ...

(defbrick Minor-Chromatic-Walkdown(2 step) Major Approach C
          (chord Dm7 1)
          (chord Dbm7 1))

(defbrick Minor-Chromatic-Walkdown(3 step) Major Approach C
        (brick Minor-Chromatic-Walkdown(2 step) Db 2)
        (chord Dbm7 1))

(defbrick Minor-Chromatic-Walkdown(4 step) Major Approach C
        (brick Minor-Chromatic-Walkdown(3 step) Ab 3)
        (chord G7 1))

(defbrick Minor-Chromatic-Walkdown-Approach Major Approach C
        (brick Minor-Chromatic-Walkdown Db 3)
        (chord G7 1))

(defbrick Minor-Dropback Minor Dropback C
          (chord Cm 1)
          (chord Am7b5 1))

(defbrick Minor-Plagal-Cadence Major Cadence C
         (brick Minor-On F 1)
         (brick Major-On C 1))

(defbrick Minor-Perfect-Cadence Minor Cadence C
          (chord G7 1)
          (chord Cm7 1))

;;; Moment's, from Coltrane's "A Moment's Notice"

(defbrick Moment's-Cadence Major Cadence C
          (brick Moment's-Approach C 4)
          (chord C 4))

(defbrick Moment's-Approach Major Approach C
          (brick Straight-Approach B 2)
          (brick Straight-Approach C 2))

;;; Night & Day

(defbrick Night-&-Day-Cadence Major Cadence C
          (chord AbM7 1)
          (chord G7 1)
          (chord C 1))

;;; Nobody

(defbrick Nobody's-Cadence Minor Cadence C
          (chord Eb 1)
          (chord G7 1)
          (chord Cm 1))

;;; Nowhere stuff (see also Somewhere stuff) from "Out of Nowhere" bVI

(defbrick Nowhere-Approach(main) Major Approach C
          (chord Ab7 1)
          (brick GenDom C 1))

(defbrick Nowhere-Approach(slow) Major Approach C
          (brick Straight-Approach G 2)
          (brick Nowhere-Approach C 2))

(defbrick Nowhere-Cadence Major Cadence C
          (brick Nowhere-Approach C 2)
          (chord C 1))

(defbrick Nowhere-Minor-Cadence(main) Minor Cadence C
          (brick Nowhere-Approach C 2)
          (chord Cm 1))

(defbrick Nowhere-Minor-Cadence(var 1) Minor Cadence C
          (brick Dominant-Cycle-2-Steps Gb 2)
          (brick Minor-On C 1)
          (chord Cm7/Bb 1))

(defbrick Nowhere-Turnaround Major Turnaround C
          (brick Dropback C 2)
          (brick Nowhere-Approach C 2))

(defbrick Nowhere-Turnaround-Minor Minor Turnaround C
          (brick Minor-Dropback C 2)
          (brick Nowhere-Approach C 2))

(defbrick Nowhere-Turnaround+On Major Cadence C
          (brick Nowhere-Turnaround C 4)
          (chord C 1))

(defbrick Nowhere-Turnaround-to-Minor-On Minor Cadence C
          (brick Nowhere-Turnaround-Minor C 4)
          (chord Cm 1))

;;; End of Nowhere stuff

;;; Passacaglia

(defbrick Passacaglia Major Turnaround Bb
        (brick On-Off-Major-bVII Bb 2)
        (brick On-Off-Major-VII Gb 2))

;;; POT = "Plain Ol' Turnaround"

(defbrick POT(main) Major Turnaround C
          (chord C 1)
          (brick GenII G 1)
          (brick Straight-Approach C 2))

(defbrick POT(var 1) Major Turnaround C
          (chord C 1)
          (chord A7 1)
          (brick Straight-Approach C 2))

(defbrick POT(var 2) Major Turnaround C
          (brick On-Off-Major-VI C 2)
          (brick Sad-Approach C 2))

(defbrick POT(var 3) Major Turnaround C
          (brick Major-On C 2)
          (brick Straight-Approach C 2))

(defbrick POT(var 4) Major Turnaround C
          (chord C 2)
          (chord Am 2)
          (chord Am/C 2)
          (chord Dm7/G 1)
          (chord G7 1))

(defbrick POT(var 5) Major Turnaround C
          (brick Dropback C 2)
          (brick Minor-On D 1)
          (chord Gsus 1))

(defbrick POT(blues) Dominant Approach C
        (chord C7 1)
        (chord A7 1)
        (chord Dm7 1)
        (chord G7 1))

(defbrick POT(cyclic) Major Turnaround C
          (chord C 1)
          (chord A7 1)
          (chord D7 1)
          (chord G7 1))

(defbrick POT(Yardbird) Major Turnaround C
          (chord C 1)
          (chord A7 1)
          (chord Dm7 2)
          (brick GenDom Eb 2))

(defbrick POT(sad approach) Major Turnaround C
          (chord C 1)
          (chord A7 1)
          (brick Sad-Approach C 2))

(defbrick POT-Spring-sub Major Turnaround C
          (chord C 1)
          (chord Am7 1)
          (chord Dm7 1)
          (brick Straight-Approach Eb 1))

(defbrick POT+On(cyclic) Major Cadence C
          (brick POT(cyclic) C 4)
          (chord C 1))

(defbrick POT Dominant Turnaround C
          (chord C7 1)
          (chord A7 1)
         (brick Straight-Approach C 2))

(defbrick Minor-POT(main) Minor Turnaround C
          (brick Minor-Dropback C 2)
          (brick Sad-Approach C 2))

(defbrick Minor-POT(var 1) Minor Turnaround C
          (brick Minor-Dropback C 2)
          (chord D7 1)
          (chord G7 1))

(defbrick Minor-POT(var 2) Minor Turnaround C
          (chord Cm 1)
          (chord Eb7 1)
          (chord Dm7b5 1)
          (chord G7 1))

(defbrick Minor-POT(var 3) Minor Turnaround G
          (brick Minor-Dropback G 2)
          (brick Straight-Approach G 2))

(defbrick Minor-POT(dominant) Minor Turnaround C
          (chord Cm 1)
          (brick Extended-Approach C 3))

(defbrick Minor-POT(sad) Minor Turnaround C
          (chord Cm 2)
          (brick Sad-Approach C 2))

(defbrick Minor-POT(altered dominant) Minor Turnaround C
          (chord Cm 1)
          (chord A7$11 1)
          (chord D7 1)
          (chord G7+ 1))

(defbrick Minor-POT+On Minor Cadence C
          (brick Minor-POT C 4)
          (chord Cm 1))

;;; End of POT stuff

;;; Pennies stuff, from "Pennies from Heaven"

(defbrick Pennies-Approach(main) Major Approach C
          (chord F 1)
          (chord Bb7 1)
          (brick Dropback C 2)
          (brick An-Approach C 2))

(defbrick Pennies-Approach(var 1) Major Approach C
          (chord F 1)
          (chord Bb7 1)
          (brick GenMajorTonic C 1)
          (brick An-Approach D 1)
          (brick An-Approach C 2))

(defbrick Pennies-Approach(var 2) Major Approach C
          (brick IV-n-Yak C 6)
          (brick Minor-Perfect-Cadence D 4)
          (brick Dominant-Cycle-2-Steps C 2))

(defbrick Pennies-Ending(main) Major Cadence C
          (brick Pennies-Approach C 6)
          (chord C 2))

(defbrick Pennies-Ending(raindrop) Major Ending C
          (chord Dm 2)
          (brick Raindrop C 2)
          (brick Straight-Approach C 2)
          (chord C 2))

(defbrick Pennies-Ending(side-slipping) Major Ending C
          (brick Major-On F 2)
          (brick Nowhere-Approach D 4)
          (brick Stablemates-Cadence C 8)
          (brick Straight-Approach C 2))

(defbrick Pennies-Ending(dropback) Major Ending C
          (brick IV-n-Yak C 3)
          (chord A7 1)
          (brick Cyclic-Approach C 2)
          (chord C 2))

(defbrick Pennies-Ending(Somewhere) Major Ending C
          (brick IV-n-Yak C 3)
          (chord D7 1)
          (brick Straight-Cadence C 3))

(defbrick Pennies-Ending(Yardbird) Major Ending C
          (brick IV-n-Yak C 5)
          (brick Minor-Perfect-Cadence A 3)
          (brick Straight-Cadence C 5))

(defbrick Pennies-Turnaround(main) Major Turnaround C
          (chord C 1)
          (chord Dm 1)
          (brick Raindrop C 2)
          (brick Straight-Approach C 4))

(defbrick Pennies-Turnaround(TTFA) Major Turnaround C
          (brick TTFA-Dropback C 4)
          (brick Straight-Approach C 4))

(defbrick Pennies-Turnaround-Two-Goes Major Turnaround C
          (chord C 1)
          (chord Dm 1)
          (brick Raindrop C 2)
          (brick Straight-Approach C 2)
          (brick Straight-Approach C 2))

;;; End of Pennies stuff

;;; Perfect

(defbrick Perfect-Cadence Major Cadence C
          (chord G7 1)
          (chord C 1))

;;; Pullbacks

(defbrick Pullback(basic) Major Pullback C
          (brick An-Approach C 2)
          (brick An-Approach D 2))

(defbrick Pullback(var 1) Major Pullback C
          (brick An-Approach C 2)
          (chord Em7 1)
          (chord Am7 1))

(defbrick Pullback(tritone) Major Pullback C
          (brick An-Approach C 2)
          (chord Bb7 1)
          (chord A7 1))

(defbrick Rainy-Pullback Major Pullback C
          (brick Straight-Approach C 1)
          (brick Raindrop C 1))

(defbrick Pullback-Extended(main) Major Pullback C
          (brick Pullback C 4)
          (brick An-Approach C 2))

(defbrick Pullback-Extended(yardbird) Major Pullback C
          (chord Dm7 1)
          (chord Bb7 1)
          (chord Em7 1)
          (chord A7  1)
          (chord Dm7 1)
          (chord G7 1))

(defbrick Pullback-to-Cadence Major Cadence C
          (brick Pullback-Extended C 6)
          (chord C 2))

(defbrick Pullback-Cadence-with-Dropback(rainy) Major Dropback C
          (brick Straight-Approach C 2)
          (brick Seven-Chord-Dropback(rainy) C 6))

;;; Rainbow

(defbrick Rainbow-Cadence(var 1) Major Cadence G
          (chord G  1)
          (chord Bm 1)
          (chord C  1))

(defbrick Rainbow-Cadence(var 2) Dominant Cadence G
          (chord G/B 1)
          (chord B+ 1)
          (chord C7$11 1))

(defbrick Rainbow-Cadence(var 3) Major Cadence G
          (brick Major-On G 1)
          (chord B+ 1)
          (brick Major-On C 1))

(defbrick Rainbow-Cadence(var 4) Major Cadence G
          (chord G 1)
          (chord B7 1)
          (chord C 1))

(defbrick Rainbow-Overrun Major Overrun C
          (chord C 1)
          (chord E7 1)
          (chord F 1)
          (chord Bb7 1))

;;; Raindrop

(defbrick Raindrop(main) Major Misc C
          (chord Em 1)
          (chord Ebdim 1))

(defbrick Raindrop(var 1) Major Misc C
          (chord C 1)
          (chord Ebdim 1))

(defbrick Raindrop(var 2) Major Misc C
          (chord A7/E 1)
          (chord Ebdim 1))

;;; Rainy

(defbrick Rainy-Turnaround Major Turnaround C
          (brick Raindrop C 1)
          (brick Straight-Approach C 1))

(defbrick Rainy-Cadence Major Cadence C
          (brick Rainy-Turnaround C 2)
          (chord C 1))

(defbrick Rainy-Cadence(minor chromatic) Major Cadence C
          (brick Minor-Chromatic-Walkdown D 1)
          (brick Straight-Cadence C 3))

;;; Reverse

(defbrick Reverse-Dominant-Cycle-2-Steps Dominant Dropback G
          (chord G7 1)
          (chord D7 1))

(defbrick Reverse-Dominant-Cycle-3-Steps Dominant Dropback G
          (brick Reverse-Dominant-Cycle-2-Steps C 2)
          (chord A7 1))

(defbrick Reverse-Dominant-Cycle-4-Steps Dominant Dropback G
          (brick Reverse-Dominant-Cycle-3-Steps C 3)
          (chord E7 1))

(defbrick Reverse-Dominant-Cycle-5-Steps Dominant Dropback G
          (brick Reverse-Dominant-Cycle-4-Steps C 4)
          (chord B7 1))

(defbrick Reverse-Dominant-Cycle-6-Steps Dominant Dropback G
          (brick Reverse-Dominant-Cycle-5-Steps C 5)
          (chord F$7 1))

;;; Rhythm

;;; Rhythm Bridge

(defbrick Rhythm-Bridge Major Approach C
          (brick GenDom A 2)
          (brick GenDom D 2)
          (brick GenDom G 2)
          (brick GenDom C 2))

(defbrick Rhythm-Turnaround(main) Major Turnaround C
          (brick Upslide C 2)
          (chord Dm7 1)
          (chord Ebo 1))

(defbrick Rhythm-Turnaround(var) Major Turnaround C
          (brick Upslide C 2)
          (chord G7/D 1)
          (chord Ebo 1))

(defbrick Rhythm-Turnaround+On Major Cadence C
          (chord Dbdim 1)
          (brick Surge D 2)
          (chord C 1))

;;; Sad

(defbrick Sad-Approach Minor Approach C
          (chord Dm7b5 1)  
          (chord G7 1))

(defbrick Sad-Cadence(main) Minor Cadence C
          (brick Sad-Approach C 2)
          (chord Cm 2))

(defbrick Sad-Cadence(var) Minor Cadence Eb
          (chord F7$11 1)
          (brick Minor-Perfect-Cadence Eb 2))

(defbrick Sad-Cadence-with-Overrun(var) Minor Overrun C
          (brick Sad-Cadence C 3)
          (chord Ab7 1))

(defbrick Sad-Dropback Major Approach C
          (chord Dm7b5 1)
          (chord G7 1)
          (chord Cm 1)
          (chord Am7b5 1)
          (chord Dm7b5 1)
          (chord G7 1))

(defbrick Sad-SPOT Minor Turnaround C
          (brick Sad-Approach D 2)
          (brick Sad-Approach C 2))

;;; End of Sad stuff

;;; Satin

(defbrick Satin-Cadence Major Cadence C
          (brick Straight-Approach G 2)
          (brick Straight-Approach Gb 2)
          (chord C 1))


;;; Side-Slip

(defbrick Side-Slip Major Approach F
          (chord C$m7b5 2)
          (chord Cm7 1)
          (chord F7 1))

(defbrick Side-Slips(2) Major Approach C
          (brick An-Approach G 2)
          (brick An-Approach Gb 2))

(defbrick Side-Slips(2 variant) Major Approach C
          (chord Em 1)
          (chord Am 1)
          (brick An-Approach C 2))

(defbrick Side-Slips(3) Major Approach C
          (brick An-Approach Ab 2)
          (brick Side-Slips(2) C 4))

(defbrick Side-Slips(4) Major Approach C
          (brick Side-Slips(2) D 2)
          (brick Side-Slips(2) C 2))

(defbrick Side-Slips(5) Major Approach C
          (brick An-Approach Bb 2)
          (brick Side-Slips(4) C 8))

(defbrick Side-Slips(6) Major Approach C
          (brick Side-Slips(2) E 4)
          (brick Side-Slips(4) C 8))

;;; Sharp Fourpenny

(defbrick Sharp-Fourpenny-Approach Major Approach C
          (chord F$m7b5 2)
          (brick Yardbird-Cadence C 4)
          (brick Straight-Approach D 2)
          (brick Straight-Approach C 4))

(defbrick Sharp-Fourpenny-Ending(main) Major Cadence C
          (brick Sharp-Fourpenny-Approach C 12)
          (chord C 4))

(defbrick Sharp-Fourpenny-Ending(var) Major Ending C
          (brick Side-Slip Bb 4)
          (brick Straight-Approach D 4)
          (brick Sus-Cadence C 8))

;;; Sixpenny

(defbrick Sixpenny-Approach Major Approach C
          (chord Am 4)
          (brick Yardbird-Cadence C 4)
          (brick Straight-Approach D 2)
          (brick Straight-Approach C 1))

(defbrick Sixpenny-Ending Major Ending C
          (brick Sixpenny-Approach C 6)
          (chord C 1))


;;; "Spring" (opposite of "Autumnal") stuff

(defbrick Spring-Approach(simple) Major Approach C
          (brick GenII A 1)
          (brick Straight-Approach C 1))

(defbrick Spring-Approach(extended) Major Approach C
          (brick GenII A 1)
          (chord E7 1)
          (brick Straight-Approach C 2))

(defbrick Spring-Cadence(main) Major Cadence C
          (brick Spring-Approach C 2)
          (chord C 1))

;;; Somewhere stuff (Tritone sub for Nowhere, II7)

(defbrick Somewhere/Nowhere-Approach Major Approach C
          (chord D7 1)
          (chord Ab7 1))

;;; End of Somewhere stuff


;;; SPOT = "Suspended Plain Ol' Turnaround" replaces the initial I with a iii

(defbrick SPOT(main) Major Turnaround C
          (brick An-Approach D 2)
          (brick Straight-Approach C 2))

(defbrick SPOT(minorIV) Major Turnaround C
          (chord Em7 1)
          (brick Extended-Approach C 3))

(defbrick SPOT(var 1) Major Turnaround C
          (chord Em7 1)
          (brick Minor-Perfect-Cadence A 3)
          (brick Straight-Approach C 4))

(defbrick SPOT(var 2) Major Turnaround C
          (brick Straight-Approach D 4)
          (brick Straight-Approach C 3)
          (chord G7/F 1))

(defbrick SPOT(var 3) Major Turnaround C
          (brick Tritone-Approach D 2)
          (brick Straight-Approach C 2))

(defbrick SPOT(sideslip) Major Turnaround C
          (chord Em7 2)
          (chord Ebm7 1)
          (chord Ab7 1)
          (chord Dm7 2)
          (chord G7 2))

(defbrick Multi-Sub-POT Major Turnaround C
          (chord C 2)
          (chord Bbm7 1)
          (chord Eb7 1)
          (chord Am7 1)
          (chord D7 1)
          (chord Abm7 1)
          (chord Db7 1))

;;; Stablemates

(defbrick Stablemates-Approach Major Approach C
          (brick Straight-Approach Db 1)
          (brick An-Approach C 1))

(defbrick Stablemates-Cadence Major Cadence C
          (brick Stablemates-Approach C 2)
          (chord C 1))


;;; Starlight stuff, from the ending of "Stella by Starlight"


(defbrick Starlight-Approach(main) Major Approach C
          (brick An-Approach E 2)
          (brick An-Approach D 2)
          (brick An-Approach C 2))

(defbrick Starlight-Approach(Dizzy) Major Approach C
          (chord F$m7b5 1)
          (chord B7 1)
          (brick Tritone-Approach D 2)
          (brick Tritone-Approach C 2))

(defbrick Starlight-Approach(Dizzy var 2) Major Approach C
          (brick Dizzy-Approach E 2)
          (brick SPOT C 4))

(defbrick Starlight-Approach(Night-&-Day) Major Approach C
          (brick Light-&-Day-Approach E 2)
          (brick SPOT C 4))

(defbrick Starlight-Approach(rainy) Major Approach C
          (chord F$m7b5 1)
          (chord Fm 1)
          (brick Rainy-Turnaround C 4))

(defbrick Starlight-Approach(tritone start) Major Approach C
          (brick Tritone-Approach E 2)
          (brick SPOT C 6))

(defbrick Starlight-Cadence(main) Major Cadence C
          (brick Starlight-Approach C 6)
          (chord C 2))

(defbrick Starlight-Cadence(airegin) Major Cadence C
          (brick Sad-Approach E 2)
          (brick Sad-Cadence D 6)
          (brick Perfect-Cadence C 4))

(defbrick Starlight-Cadence(tension) Dominant Cadence C
          (brick Starlight-Approach C 6)
          (chord C7 2))

(defbrick Starlight-Cadence(Night-&-Day) Major Cadence C
          (brick Starlight-Approach(Night-&-Day) C 6)
          (chord C 2))

(defbrick Starlight-Dropback Major Dropback C
          (brick Sad-Approach E 2)
          (brick Straight-Approach D 2))

(defbrick Starlight-Opening Major Opening Bb
          (brick Sad-Approach D 2)
          (brick Straight-Approach Bb 2))

;;; End of Starlight stuff


;;; Straight (meaning Major) stuff

(defbrick Straight-Approach(main) Major Approach C
          (chord Dm7 1)
          (chord G7 1))

(defbrick Straight-Approach(var) Major Approach F
          (chord Gm7 2)
          (chord C7 1)
          (chord C7/Bb 1))

(defbrick Straight-Cadence(main) Major Cadence C
          (brick Straight-Approach C 2)
          (chord C 2))

(defbrick Straight-Cadence-with-Dropback Major Dropback C
          (brick Straight-Approach C 2)
          (chord C 1)
          (chord Am7 1))

(defbrick Straight-Cadence-+-Dominant-Overrun Major Overrun C
          (brick Straight-Cadence C 3)
          (chord F7 1))

;;; Surge

(defbrick Surge(major) Major Misc C
          (brick Major-On C 1)
          (chord C$o 1))

(defbrick Surge(minor) Minor Misc C
          (brick Minor-On C 1)
          (chord C$o 1))

(defbrick Surge(dominant) Dominant Misc C
          (chord C7 1)
          (chord C$o 1))

(defbrick Surge(major var) Major Misc C
          (brick Surge(major) C 1)
          (chord G/D 1))

(defbrick Surge(minor var) Minor Misc C
          (brick Surge(minor) C 1)
          (chord G/D 1))

(defbrick Surge(dominant var) Dominant Misc C
          (brick Surge(dominant) C 1)
          (chord G/D 1))

;;; Supertension (7$4 or 7$11)

(defbrick Supertension-Ending Dominant Cadence C
          (brick Straight-Approach C 2)
          (chord C7$4 2))

;;; Surprise, meaning major approach with minor resolution, or vice-versa

(defbrick Surprise-Minor-Cadence Minor Cadence C
          (brick Straight-Approach C 2)
          (chord Cm 2))

(defbrick Surprise-Major-Cadence Major Cadence C
         (brick Sad-Approach C 2)
         (chord C 2))

;;; Sus = Suspended fourth stuff

(defbrick Sus-Approach Major Approach C
          (brick Major-On D 1)
          (chord D7 1)
          (brick Major-On G 2))

(defbrick Sus-Approach(var 1) Major Approach C
          (brick Minor-On D 1)
          (chord Gsus 1))

(defbrick Sus-Cadence Major Cadence F
          (chord Gm7 1)
          (chord Gm7/C 1)
          (brick Major-On F 1))

(defbrick Sus-Cadence(main) Major Cadence C
          (chord G7sus 1)
          (chord C 1))

(defbrick Sus-Cadence(var 1) Major Cadence C
          (chord Dm7 1)
          (chord G7sus 1)
          (chord C 1))

(defbrick Sus-Cadence(var 2) Major Cadence C
          (chord G7sus 1)
          (chord G7 1)
          (chord C 1))

;;; Tension

(defbrick Tension-Cadence Dominant Cadence Bb
          (chord Cm7 1)
          (chord F7 1)
          (chord Bb7 2))

(defbrick Tension-Pullback Dominant Approach C
          (brick Straight-Approach C 2)
          (brick Dominant-Cycle-2-Steps D 2))

(defbrick Tension-SPOT Major Turnaround G
          (brick Dominant-Cycle-2-Steps A 2)
          (brick Straight-Approach G 2))

;;; "To ..." stuff (II, IV, Somewhere, ...)
 
(defbrick To-II-n-Back Major Cadence C
          (chord C 1)
          (brick Minor-On D 1)
          (chord Ebm7b5 1)
          (brick Major-On C 1))

(defbrick To-Somewhere Major Approach C
          (chord C 1)
          (chord D7 1))

;;; Here "Cadence" means one of a variety of cadences

(defbrick Cadence(perfect) Major Invisible C
          (brick Perfect-Cadence C 2))

(defbrick Cadence(straight) Major Invisible C
          (brick Straight-Cadence C 2))

(defbrick Cadence(sad) Major Invisible C
          (brick Sad-Cadence C 2))

(defbrick Cadence(tritone) Major Invisible C
          (brick Tritone-Cadence C 2))

(defbrick Cadence(surge) Major Invisible C
          (brick TonicOrDom F 1)
          (chord F$o 1)
          (chord C 1))

(defbrick To-IV-n-Bird-POT Major Turnaround C
          (chord C 1)
          (brick GenDom F 1)
          (chord FM7 1)
          (chord Bb7 1)
          (brick POT C 4))

;;; TTFA = Turnaround to Further Away 
;;; where "Further Away" means the ii chord, so these are Dropbacks ending on VI7

(defbrick TTFA-Dropback(main) Major Dropback C
          (chord C 2)
          (brick Straight-Approach D 2))

(defbrick TTFA-Dropback(dropback) Major Dropback C
          (chord C/G 1)
          (brick Minor-Perfect-Cadence E 2)
          (chord A7 1))

(defbrick TTFA-Dropback(IV-variant) Major Dropback C
          (chord C 1)
          (brick TonicOrDom F 1)
          (brick Liberal-Approach D 2))

(defbrick TTFA-Dropback(ii-variant) Major Dropback C
          (chord C 1)
          (chord Dm7 1)
          (brick Straight-Approach D 2))

(defbrick TTFA-Dropback(II n Back variant) Major Dropback C
          (brick Major-On C 1)
          (brick II-n-Back C 2)
          (chord Am7 1))

(defbrick TTFA-Dropback(rainy) Major Dropback C
          (chord C 1)
          (chord F7 1)
          (chord Em7 1)
          (chord Ebo 1))

(defbrick TTFA-Dropback(minor chromatic descent) Major Dropback C
          (chord C 1)
          (chord F7 1)
          (brick Minor-Chromatic-Walkdown D 2))

;;; End of TTFA stuff


;;; Twopenny stuff

(defbrick Twopenny-Approach Major Approach C
          (chord Dm 2)
          (brick Yardbird-Cadence C 4)
          (brick Straight-Approach D 2)
          (brick Straight-Approach C 4))

(defbrick Twopenny-Ending(main) Major Ending C
          (brick Twopenny-Approach C 12)
          (chord C 4))

(defbrick Twopenny-Ending(Nowhere dropback) Major Approach F
          (brick Yardbird-Sub-Cadence F 6)
          (chord Abm7 1)
          (chord Db7 1)
          (chord Gm7 2)
          (chord C7 2)
          (chord FM7 1))

(defbrick Twopenny-Ending(Starlight back) Major Ending F
          (brick Yardbird-Sub-Cadence F 6)
          (chord Bm7b5 1)
          (chord Bb7 1)
          (chord Am7 1)
          (chord D7 1)
          (chord Gm7 1)
          (chord C7 1)
          (chord FM7 1))

(defbrick Twopenny-Ending(var 1) Major Ending G
          (chord Am7 2)
          (brick GenDom Bb 2)
          (brick Straight-Approach A 2)
          (brick Stablemates-Cadence G 8))

(defbrick Twopenny-Ending(var 2) Major Ending C
          (chord Dm7 1)
          (chord B7 1)
          (chord Em7 1)
          (chord A7 1)
          (brick An-Approach C 2)
          (chord C 1))

(defbrick Twopenny-Ending(var 3) Major Ending Eb
          (brick II-n-Bird-SPOT Eb 12)
          (chord EbM7 2))

;;; End of Twopenny stuff

(defbrick Upslide Major Invisible C
          (chord C 1)
          (chord Dbdim 1))

;;; Tritone

(defbrick Tritone-Approach Major Approach C
          (chord Dm7 1)
          (chord Db7 1))

(defbrick Tritone-Cadence(main) Major Cadence C
          (chord Dm7 1)
          (chord Db7 1)
          (chord C 1))

(defbrick Tritone-Cadence(short) Major Cadence C
          (chord Db7 1)
          (chord C 1))

(defbrick Tritone/Straight-Approach Major Approach F
          (brick Tritone-Approach G 4)
          (brick Straight-Approach F 3)
          (chord C7/Bb 1))

;;; Two-Goes stuff, including Three-Goes and Four-Goes

(defbrick Two-Goes-Approach Major Approach F
          (brick An-Approach F 2)
          (brick An-Approach F 2))

(defbrick Two-Goes-Nowhere-Approach Major Approach C
          (brick Nowhere-Approach C 2)
          (brick Nowhere-Approach C 2))

(defbrick Two-Goes-Nowhere-Cadence Major Cadence C
          (brick Two-Goes-Nowhere-Approach C 4)
          (chord C 1))

(defbrick Two-Goes-Pullback Major Approach C
          (brick Pullback(basic) C 1)
          (brick Pullback(basic) C 1))

(defbrick Two-Goes-Pullback-Extended Major Approach C
          (brick Two-Goes-Pullback C 4)
          (brick Straight-Approach C 2))

(defbrick Two-Goes-Pullback+Cadence Major Cadence C
          (brick Two-Goes-Pullback-Extended C 6)
          (chord C 2))

(defbrick Two-Goes-Rainy-Turnaround Major Turnaround Bb
          (brick Rainy-Turnaround Bb 4)
          (brick Rainy-Turnaround Bb 4))

(defbrick Two-Goes-Sad-Approach Minor Approach C
          (brick Sad-Approach C 1)
          (brick Sad-Approach C 1))

(defbrick Two-Goes-Sad-Cadence Minor Cadence C
          (brick Two-Goes-Sad-Approach C 2)
          (chord Cm 1))

(defbrick Two-Goes-Starlight-Approach Major Approach Eb
          (brick Straight-Approach G 2)
          (brick Starlight-Approach Eb 6))

(defbrick Two-Goes-Straight-Approach Major Approach C
          (brick Straight-Approach C 1)
          (brick Straight-Approach C 1))

(defbrick Two-Goes-Straight-Cadence Major Cadence C
          (brick Two-Goes-Straight-Approach C 2)
          (chord C 1))

(defbrick Two-Goes-Straight-Cadence-with-Overrun(var 1) Major Cadence C
          (brick Two-Goes-Straight-Cadence C 5)
          (chord F 1))

(defbrick Two-Goes-Straight-Cadence-with-Overrun(var 2) Major Cadence C
          (brick Two-Goes-Straight-Cadence C 5)
          (chord F7 1))

(defbrick Three-Goes-Pullback Major Approach C
          (brick Two-Goes-Pullback C 1)
          (brick Pullback C 1))

(defbrick Three-Goes-Pullback-Extended Major Approach C
          (brick Three-Goes-Pullback C 6)
          (brick Straight-Approach C 2))

(defbrick Three-Goes-Pullback+Cadence Major Cadence C
          (brick Three-Goes-Pullback-Extended C 6)
          (chord C 2))

(defbrick Three-Goes-Sad-Approach Minor Approach C
          (brick Two-Goes-Sad-Approach C 1)
          (brick Sad-Approach C 1))

(defbrick Three-Goes-Sad-Cadence Minor Cadence C
          (brick Three-Goes-Sad-Approach C 3)
          (chord Cm 1))

(defbrick Three-Goes-Straight-Approach Major Approach C
          (brick Two-Goes-Straight-Approach C 2)
          (brick Straight-Approach C 1))

(defbrick Three-Goes-Straight-Cadence Major Cadence C
          (brick Three-Goes-Straight-Approach C 3)
          (chord C 1))

(defbrick Three-Goes-Nowhere-Approach Major Approach C
          (brick Two-Goes-Nowhere-Approach C 2)
          (brick Nowhere-Approach C 2))

(defbrick Three-Goes-Nowhere-Cadence Major Cadence C
          (brick Three-Goes-Nowhere-Approach C 4)
          (chord C 1))

(defbrick Four-Goes-Straight-Approach Major Approach C
          (brick Two-Goes-Straight-Approach C 2)
          (brick Two-Goes-Straight-Approach C 2))

(defbrick Four-Goes-Straight-Cadence Major Cadence C
          (brick Four-Goes-Straight-Approach C 4)
          (chord C 1))

(defbrick Four-Goes-Sad-Approach Minor Approach C
          (brick Two-Goes-Sad-Approach C 1)
          (brick Two-Goes-Sad-Approach C 1))

(defbrick Four-Goes-Sad-Cadence Minor Cadence C
          (brick Four-Goes-Sad-Approach C 4)
          (chord Cm 1))

(defbrick Two-Goes-Tritone-Approach Major Approach C
          (brick Tritone-Approach C 1)
          (brick Tritone-Approach C 1))

(defbrick Two-Goes-Tritone-Cadence Major Cadence C
          (brick Two-Goes-Tritone-Approach C 2)
          (chord C 1))

(defbrick Three-Goes-Tritone-Approach Major Approach C
          (brick Two-Goes-Tritone-Approach C 2)
          (brick Tritone-Approach C 1))


(defbrick Three-Goes-Tritone-Cadence Major Cadence C
          (brick Three-Goes-Tritone-Approach C 3)
          (chord C 1))

(defbrick Four-Goes-Tritone-Approach Major Approach C
          (brick Three-Goes-Tritone-Approach C 3)
          (brick Tritone-Approach C 1))

(defbrick Four-Goes-Tritone-Cadence Major Cadence C
          (brick Four-Goes-Tritone-Approach C 4)
          (chord C 1))

(defbrick Two-Goes-Dominant-Approach Major Approach C
          (brick Dominant-Cycle-2-Steps C 1)
          (brick Dominant-Cycle-2-Steps C 1))

(defbrick Two-Goes-Tritone-Straight-Approach Major Approach C
          (brick Tritone-Approach C 1)
          (brick Straight-Approach C 1))

(defbrick Two-Goes-Tritone-Straight-Cadence Major Cadence C
          (brick Two-Goes-Tritone-Straight-Approach C 4)
          (chord C 1))

;;; End of Two-Goes, Three-Goes, Four-Goes


;;; Whoopee, from "Makin' Whoopee"

(defbrick Whoopee-Turnaround Major Turnaround C
          (brick Upslide C 2)
          (brick Straight-Approach C 2))

(defbrick Whoopee-Cadence Major Cadence C
          (chord Dbdim 1)
          (brick Straight-Cadence C 3))


;;; Wonderful

(defbrick Wonderful-Opening Major Opening Bb
        (brick Surge Bb 4)
        (brick Straight-Cadence Bb 4))

;;; Yardbird

(defbrick Yardbird-Approach Major Approach C
          (chord Fm7 1)
          (chord Bb7 1))

(defbrick Yardbird-Cadence(main) Major Cadence C
          (brick Yardbird-Approach C 1)
          (chord C 1))

(defbrick Yardbird-Cadence(var) Major Cadence C
          (chord Bb7 1)
          (chord C 1))

(defbrick Yardbird-Sub-Cadence Major Cadence C
          (chord Dm7 1)
          (brick GenDom Eb 1)
          (chord C 1))

(defbrick Yardbird-Sub-Turnaround Major Turnaround C
          (chord C 1)
          (brick Minor-On A 1)
          (chord Bb7 2))


;;; On-off (replaced by XS)

(defbrick Major-On Major On C
          (chord C 1))

(defbrick Minor-On(main) Minor On C
         (chord Cm 1))

(defbrick Minor-On(var 1) Minor On C
          (chord CmM7 1))

(defbrick Minor-On(var 2) Minor On C
          (chord Cm6 1))

(defbrick Minor-On(var 3) Minor On C
          (chord Cm7 1))

(defbrick On-Off-Major-V Major On-Off C
          (chord C 1)
          (chord G7 1))

(defbrick On-Off-Major-bII Major On-Off C
          (brick Major-On C 1)
          (brick Off Db 1))

(defbrick On-Off-Major-Somewhere Major On-Off C
          (brick Major-On C 1)
          (brick Off D 1)) 

(defbrick On-Off-Major-bIII Major On-Off C
          (brick Major-On C 1)
          (brick Off Eb 1))

(defbrick On-Off-Major-III Major On-Off C
          (brick Major-On C 1)
          (brick Off E 1))

(defbrick On-Off-Major-IV Major On-Off C
          (brick Major-On C 1)
          (brick Off F 1))

(defbrick On-Off-Major-$IV Major On-Off C
          (brick Major-On C 1)
          (brick Off F$ 1))

(defbrick On-Off-Major-Nowhere Major On-Off C
          (brick Major-On C 1)
          (brick Off Ab 1))

(defbrick On-Off-Major-VI Major On-Off C
          (brick Major-On C 1)
          (brick Off A 1))

(defbrick On-Off-Major-bVII Major On-Off C
          (brick Major-On C 1)
          (brick Off Bb 1))

(defbrick On-Off-Major-VII Major On-Off C
          (brick Major-On C 1)
          (brick Off B 1))

(defbrick On-Off-Minor-bII Minor On-Off C
          (brick Minor-On C 1)
          (brick Off Db 1))

(defbrick On-Off-Minor-Somewhere Minor On-Off C
          (brick Minor-On C 1)
          (brick Off D 1)) 

(defbrick On-Off-Minor-bIII Minor On-Off C
          (brick Minor-On C 1)
          (brick Off Eb 1))

(defbrick On-Off-Minor-III Minor On-Off C
          (brick Minor-On C 1)
          (brick Off E 1))

(defbrick On-Off-Minor-IV Minor On-Off C
          (brick Minor-On C 1)
          (brick Off F 1))

(defbrick On-Off-Minor-$IV Minor On-Off C
          (brick Minor-On C 1)
          (brick Off F$ 1))

(defbrick On-Off-Minor-V Minor On-Off C
          (brick GenMinor C 1)
          (chord G7 1))

(defbrick On-Off-Minor-Nowhere Minor On-Off C
          (brick Minor-On C 1)
          (brick Off Ab 1))

(defbrick On-Off-Minor-VI Minor On-Off C
          (brick Minor-On C 1)
          (brick Off A 1))

(defbrick On-Off-Minor-bVII Minor On-Off C
          (brick Minor-On C 1)
          (brick Off Bb 1))

(defbrick On-Off-Minor-VII Minor On-Off C
          (brick Minor-On C 1)
          (brick Off B 1))

(defbrick Off-On-Major-bII Major Off-On C
          (brick Off Db 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-Somewhere Major Off-On C
          (brick Off D 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-bIII Major Off-On C
          (brick Off Eb 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-III Major Off-On C
          (brick Off E 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-IV Major Off-On C
          (brick Off F 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-$IV Major Off-On C
          (brick Off F$ 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-Nowhere Major Off-On C
          (brick Off Ab 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-VI Major Off-On C
          (brick Off A 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-bVII Major Off-On C
          (brick Off Bb 1)
          (brick Major-On C 1))

(defbrick Off-On-Major-VII Major Off-On C
          (brick Off B 1)
          (brick Major-On C 1))

(defbrick Off-On-Minor-bII Minor Off-On C
          (brick Off Db 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-Somewhere Minor Off-On C
          (brick Off D 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-bIII Minor Off-On C
          (brick Off Eb 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-III Minor Off-On C
          (brick Off E 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-IV Minor Off-On C
          (brick Off F 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-$IV Minor Off-On C
          (brick Off F$ 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-Nowhere Minor Off-On C
          (brick Off Ab 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-VI Minor Off-On C
          (brick Off A 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-bVII Minor Off-On C
          (brick Off Bb 1)
          (brick Minor-On C 1))

(defbrick Off-On-Minor-VII Minor Off-On C
          (brick Off B 1)
          (brick Minor-On C 1))

(defbrick On-Off-Twice-To-V Major On-Off+ C
          (brick Major-On C 1)
          (chord G7 1)
          (brick Major-On C 1)
          (chord G7 1))

(defbrick On-Off-On-To-IV-Major Major On-Off+ C
          (brick Major-On C 1)
          (brick Amen-Cadence C 2))

(defbrick On-Off-On-+Dropback-IV-Major Major Dropback C
          (brick On-Off-On-To-IV-Major C 3)
          (brick SuperGenDom D 1))

(defbrick On-Off-On-To-IV-Minor Minor On-Off+ C
          (brick Minor-On C 1)
          (chord Fm 1)
          (brick Minor-On C 1))

(defbrick On-Off-Twice-On-To-IV Major On-Off+ C
          (brick On-Off-On-To-IV-Major C 4)
          (brick Amen-Cadence C 2))

(defbrick On-Off-Thrice-To-IV Major On-Off+ C
          (brick On-Off-Major-IV C 2)
          (brick On-Off-Major-IV C 2)
          (brick On-Off-Major-IV C 2))

(defbrick On-Off-Thrice-On-To-IV Major On-Off+ C
          (brick On-Off-Twice-On-To-IV C 4)
          (brick Amen-Cadence C 2))

(defbrick On-Off-Four-Times-On-To-IV Major On-Off+ C
          (brick On-Off-Thrice-On-To-IV C 4)
          (brick Amen-Cadence C 2))

(defbrick On-Off-On-To-V(main) Major On-Off+ C
          (brick Major-On C 1)
          (brick Perfect-Cadence C 2))

(defbrick On-Off-On-To-V(minor) Minor On-Off+ C
          (brick Minor-On C 1)
          (brick DomOrMinor C 1)
          (brick Minor-On C 1))

(defbrick On-Off-On-To-V(sus) Major On-Off+ C
          (brick Major-On C 1)
          (chord Gsus 1)
          (brick Major-On C 1))

(defbrick On-Off-Twice-On-To-V Major On-Off+ C
          (brick On-Off-On-To-V C 4)
          (brick Perfect-Cadence C 2))

(defbrick On-Off-Thrice-On-To-V Major On-Off+ C
          (brick On-Off-Twice-On-To-V C 4)
          (brick Perfect-Cadence C 2))

(defbrick On-Off-Four-Times-On-To-V Major On-Off+ C
          (brick On-Off-Thrice-On-To-V C 4)
          (brick Perfect-Cadence C 2))

(defbrick On-Off-On-To-II Major On-Off+ C
          (brick Major-On C 1)
          (chord Dm7 1)
          (brick Major-On C 1))

(defbrick On-Off-Twice-To-II Major On-Off+ C
          (brick Major-On C 1)
          (chord Dm7 1)
          (brick Major-On C 1)
          (chord Dm7 1))
 
(defbrick On-Off-Twice-On-To-II Major On-Off+ C
          (brick On-Off-On-To-II C 4)
          (chord Dm7 1)
          (brick Major-On C 1))

(defbrick On-Off-Thrice-On-To-II Major On-Off+ C
          (brick On-Off-Twice-On-To-II C 4)
          (chord Dm7 1)
          (brick Major-On C 1))

(defbrick On-Off-Four-Times-On-To-II Major On-Off+ C
          (brick On-Off-Thrice-On-To-II C 4)
          (chord Dm7 1)
          (brick Major-On C 1))

(defbrick On-Off-To-IV-Twice-Major-then-Minor Minor On-Off+ C
          (brick On-Off-Major-IV C 1)
          (brick On-Off-Minor-IV C 1))

(defbrick On-Off-Major-Minor-V Minor On-Off+ C
          (brick On-Off-Major-V C 1)
          (brick On-Off-Minor-V C 1))

(defbrick On-Off-Twice-Minor-V Minor On-Off+ C
          (brick On-Off-Minor-V C 1)
          (brick On-Off-Minor-V C 1))

(defbrick On-Off-Thrice-Minor-V Minor On-Off+ C
          (brick On-Off-Twice-Minor-V C 2)
          (brick On-Off-Minor-V C 1))

(defbrick On-Off-On-Minor-V Minor On-Off+ C
          (brick On-Off-Minor-V C 1)
          (brick GenMinor C 1))

(defbrick On-Off-On-Twice-Minor-V Minor Cadence C
          (brick On-Off-Twice-Minor-V C 1)
          (brick GenMinor C 1))

(defbrick On-Off-On-To-bVII Major On-Off+ C
          (brick Major-On C 1)
          (chord Bb7 1)
          (brick Major-On C 1))
 
(defbrick On-Off-Twice-On-To-bVII Major On-Off+ C
          (brick On-Off-On-To-bVII C 4)
          (chord Bb7 1)
          (brick Major-On C 1))

(defbrick On-Off-Thrice-On-To-bVII Major On-Off+ C
          (brick On-Off-Twice-On-To-bVII C 4)
          (chord Bb7 1)
          (brick Major-On C 1))

(defbrick On-Off-Four-Times-On-To-bVII Major On-Off+ C
          (brick On-Off-Thrice-On-To-bVII C 4)
          (chord Bb7 1)
          (brick Major-On C 1))

;;; User-defined bricks

(defbrick Diatonic-I-ii-iii Major Approach G
        (brick Major-On G 1)
        (brick Minor-On A 1)
        (brick Minor-On B 1))

(defbrick Diatonic-I-ii-iii-vi Major Approach G
        (chord GM7 1)
        (chord Am7 1)
        (chord Bm7 1)
        (chord Em7 1))

(defbrick On-Off-to-bVII Major On-Off F
        (brick Major-On F 1)
        (brick MajorOrDom Eb 1))

(defbrick On-Off-Twice-to-bVII Major On-Off F
        (brick On-Off-to-bVII F 2)
        (brick On-Off-to-bVII F 2))

(defbrick To-IV-n-Back(blues) Major Cadence F
        (chord F7_ 1)
        (brick IV-n-Back(blues) F 2))

(defbrick IV-n-Back(blues) Major Cadence F
        (chord Bb7 1)
        (chord Bo7 1)
        (chord F7_ 1))

(defbrick Diatonic-Walk-Down Major Cadence C
        (chord Em7 1)
        (chord Dm7 1)
        (chord C   1))

(defbrick Minor-ii-to-Minor-v Major Approach C
        (brick On-Off-Minor-Somewhere D 3)
        (brick Off-On-Minor-VII A 3)
        (brick Minor-On G 2))

(defbrick To-IV-+-Bird-Approach Major Approach Bb
        (chord Bb 1)
        (chord Bb7 1)
        (chord Eb 1)
        (chord Ab7 1))

(defbrick Daahoud-Cadence Major Cadence Eb
        (chord Abm7 1)
        (chord Gb7 1)
        (chord F7 1)
        (chord EM7 1)
        (chord EbM7 4))

(defbrick Rainy-Pullback-Extended Dominant Approach C
        (brick Rainy-Pullback C 4)
        (brick Straight-Approach C 2))

(defbrick Rainbow-Overrun(sub 1) Major Overrun Eb
        (brick Minor-On G 1)
        (brick Off-On-Major-VII Ab 2)
        (brick Minor-On Ab 1))

(defbrick Major-Off-On(ii-I) Major Cadence Eb
        (chord Fm7b5 1)
        (brick Major-On Eb 1))

(defbrick Major-Off-On(iv-I) Major Cadence Eb
        (chord Abm 1)
        (brick Major-On Eb 1))

(defbrick Major-Off-On-Off Major On-Off+ Eb
        (brick Major-Off-On Eb 2)
        (chord Fm7b5 1))

(defbrick Diatonic-ii-I Major Cadence Eb
        (brick Minor-On F 1)
        (brick Major-On Eb 1))

(defbrick Approach-from-iii Minor Approach F
        (brick Sad-Cadence G 3)
        (brick Nowhere-Approach F 3))

(defbrick Four-Star-Ending(var 1) Major Ending Eb
        (brick Major-On Ab 2)
        (brick Approach-from-iii F 6)
        (brick Body-&-Soul-Cadence Eb 6))

(defbrick Major-On-+-Dropback Major Dropback C
        (chord C6 1)
        (chord Em7b5/Bb 1)
        (chord A7 2))

(defbrick Descending-Minor-CESH-Approach Minor Approach G
        (chord Gm7 1)
        (chord Gm/F$ 1)
        (chord Gm7/F 1)
        (chord C7 1))

(defbrick On-+-Dropback Major Dropback Eb
        (brick Major-On Eb 1)
        (brick Gen7 C 1))

(defbrick Tritone-Sub-Turnaround Major Turnaround Eb
        (brick On-+-Dropback Eb 2)
        (brick Tritone-Approach Eb 2))

(defbrick Somewhere-Turnaround Major Turnaround F
        (brick Donna-Lee-Start F 2)
        (brick Straight-Approach F 2))

(defbrick Giant-Step-Turnaround(var 1) Major Turnaround Eb
        (brick Giant-Step B 6)
        (chord Bb7sus4 1)
        (chord Bb7 1))

(defbrick Giant-Step-Turnaround(var 2) Major Turnaround Eb
        (brick Giant-Step B 6)
        (chord Bb7sus4 1)
        (chord Bb7 1))

(defbrick Seven-sus4-to-3 Major Approach Eb
        (chord Bb7sus4 1)
        (chord Bb7 1))


;;; default brick definitions (generated from )
(def-default-brick GenMinor GenMinor_var1)
(def-default-brick GenDom GenDom_var1)
(def-default-brick GenII GenII_var1)
(def-default-brick LiberalII LiberalII_var1)
(def-default-brick GenIII GenIII_var1)
(def-default-brick GenVI GenVI_var1)
(def-default-brick Gen7 Gen7_var1)
(def-default-brick Liberal-Approach Liberal-Approach_var1)
(def-default-brick SuperGenDom SuperGenDom_var1)
(def-default-brick GenMajorTonic GenMajorTonic_main)
(def-default-brick Off Off_dominant)
(def-default-brick TonicOrDom TonicOrDom_major)
(def-default-brick MajorOrDom MajorOrDom_major)
(def-default-brick DomOrMinor DomOrMinor_minor)
(def-default-brick Ascending-Minor-CESH Ascending-Minor-CESH_3step)
(def-default-brick Descending-Minor-CESH Descending-Minor-CESH_3step)
(def-default-brick Ascending-Major-CESH Ascending-Major-CESH_3step)
(def-default-brick Ascending-&-Descending-Minor-CESH Ascending-&-Descending-Minor-CESH_4step)
(def-default-brick Seven-Chord-Dropback Seven-Chord-Dropback_main)
(def-default-brick Autumn-Leaves-Opening Autumn-Leaves-Opening_main)
(def-default-brick Autumnal-Approach Autumnal-Approach_var1)
(def-default-brick Autumnal-Cadence Autumnal-Cadence_main)
(def-default-brick Body-&-Soul-Approach Body-&-Soul-Approach_main)
(def-default-brick Body-&-Soul-Cadence Body-&-Soul-Cadence_major)
(def-default-brick Chromatic-Dropback Chromatic-Dropback_main)
(def-default-brick Chromatic-Minor-Ascending Chromatic-Minor-Ascending_3steps)
(def-default-brick Chromatic-Minor-Descending Chromatic-Minor-Descending_3steps)
(def-default-brick Chromatically-Descending-Dominants Chromatically-Descending-Dominants_3steps)
(def-default-brick Cyclic-Approach Cyclic-Approach_2)
(def-default-brick Diatonic-Walkup Diatonic-Walkup_3stepsfromii)
(def-default-brick Dizzy-Approach Dizzy-Approach_var1)
(def-default-brick Dizzy-Cadence Dizzy-Cadence_main)
(def-default-brick Dogleg-Approach Dogleg-Approach_short)
(def-default-brick Dogleg-Cycle Dogleg-Cycle_3steps)
(def-default-brick Dominant-Cycle Dominant-Cycle_Two-step)
(def-default-brick Donna-Lee-Start Donna-Lee-Start_var1)
(def-default-brick Donna-Lee-Opening Donna-Lee-Opening_main)
(def-default-brick Dropback Dropback_main)
(def-default-brick Dropback-Approach Dropback-Approach_main)
(def-default-brick Extended-Approach Extended-Approach_var1)
(def-default-brick Foggy-Turnaround Foggy-Turnaround_main)
(def-default-brick Honeysuckle-Bridge Honeysuckle-Bridge_main)
(def-default-brick II-n-Back II-n-Back_main)
(def-default-brick II-n-Bird-SPOT II-n-Bird-SPOT_main)
(def-default-brick To-IV To-IV_main)
(def-default-brick IV-n-Bird-SPOT IV-n-Bird-SPOT_main)
(def-default-brick Ladybird-Turnaround Ladybird-Turnaround_main)
(def-default-brick Long-Approach Long-Approach_main)
(def-default-brick Long-Cadence Long-Cadence_main)
(def-default-brick Minor-Chromatic-Walkdown Minor-Chromatic-Walkdown_2step)
(def-default-brick Moment Moment_quotes-Cadence)
(def-default-brick Nobody Nobody_quotes-Cadence)
(def-default-brick Nowhere-Approach Nowhere-Approach_main)
(def-default-brick Nowhere-Minor-Cadence Nowhere-Minor-Cadence_main)
(def-default-brick POT POT_main)
(def-default-brick POT+On POT+On_cyclic)
(def-default-brick Minor-POT Minor-POT_main)
(def-default-brick Pennies-Approach Pennies-Approach_main)
(def-default-brick Pennies-Ending Pennies-Ending_main)
(def-default-brick Pennies-Turnaround Pennies-Turnaround_main)
(def-default-brick Pullback Pullback_basic)
(def-default-brick Pullback-Extended Pullback-Extended_main)
(def-default-brick Pullback-Cadence-with-Dropback Pullback-Cadence-with-Dropback_rainy)
(def-default-brick Rainbow-Cadence Rainbow-Cadence_var1)
(def-default-brick Raindrop Raindrop_main)
(def-default-brick Rhythm-Turnaround Rhythm-Turnaround_main)
(def-default-brick Sad-Cadence Sad-Cadence_main)
(def-default-brick Sad-Cadence-with-Overrun Sad-Cadence-with-Overrun_var)
(def-default-brick Side-Slips Side-Slips_2)
(def-default-brick Sharp-Fourpenny-Ending Sharp-Fourpenny-Ending_main)
(def-default-brick Spring-Approach Spring-Approach_simple)
(def-default-brick Spring-Cadence Spring-Cadence_main)
(def-default-brick SPOT SPOT_main)
(def-default-brick Starlight-Approach Starlight-Approach_main)
(def-default-brick Starlight-Cadence Starlight-Cadence_main)
(def-default-brick Straight-Approach Straight-Approach_main)
(def-default-brick Straight-Cadence Straight-Cadence_main)
(def-default-brick Surge Surge_major)
(def-default-brick Cadence Cadence_perfect)
(def-default-brick TTFA-Dropback TTFA-Dropback_main)
(def-default-brick Twopenny-Ending Twopenny-Ending_main)
(def-default-brick Tritone-Cadence Tritone-Cadence_main)
(def-default-brick Two-Goes-Straight-Cadence-with-Overrun Two-Goes-Straight-Cadence-with-Overrun_var1)
(def-default-brick Yardbird-Cadence Yardbird-Cadence_main)
(def-default-brick Minor-On Minor-On_main)
(def-default-brick On-Off-On-To-V On-Off-On-To-V_main)
(def-default-brick Major-Off-On Major-Off-On_ii-I)
(def-default-brick Giant-Step-Turnaround Giant-Step-Turnaround_var1)


;;; Returns a list of default brick definitions from the defbrick code
;;; You must wrap a quote and parens around the defbrick definitions
;;; (yuck!), then pull out the resulting definitions.  If we want to
;;; automate the process of converting the Impro-Visor My.dictionary
;;; file to Scheme, we should write a program that reads from
;;; My.dictionary, converst Java comments to Scheme comments, converts
;;; # and names beginning with numbers to Schemely equivalents, then
;;; generates my.dictionary in terms of defbrick and def-default-brick
;;; statements.  Then we can just load the my-dictionary.scm file to
;;; get all the variable definitions.
;;;
;;; Another issue is that all these brick names pollute the name
;;; space.  Libraries or modules would help with this.
(define list-default-brick-definitions
  (lambda (ls)
    (let loop ([ls ls][already-seen '()][acc '()])
      (pmatch ls
        [() (reverse acc)]
        [((defbrick ,name ,variant . ,body) . ,rest) (guard (pair? variant))
         (cond
           [(memv name already-seen)
            (loop rest already-seen acc)]
           [else
            (let ([acc (cons `(define ,name ,(qualified-name name variant)) acc)])
              (loop rest (cons name already-seen)  acc))])]
        [((defbrick ,name . ,body) . ,rest)
         (cond
           [(memv name already-seen)
            (loop rest already-seen acc)]
           [else
            (loop rest (cons name already-seen) acc)])]))))
