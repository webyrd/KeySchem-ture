;;; Test file for cfgnorm.scm

;;; run tests
(load "my-dictionary.scm")
(load "prelude.scm")
(load "cfgnorm.scm")

;;; Tests for grammars using "new hotness" representation of bricks (as individual
;;; tagged lists instead of association lists).

(define get-bricks-from-rules* (lambda (ls) ((get-foo-from-rules* brick?) ls)))
(define get-symbols-from-rules* (lambda (ls) ((get-foo-from-rules*
                                          (lambda (x)
                                            (and (symbol? x)
                                                 (not (eq? x '->)))))
                                         ls)))
(define get-foo-from-rules*
  (lambda (pred)
    (letrec ([get-foo*
              (lambda (ls)
                (cond
                  [(null? ls) '()]
                  [(pred (car ls))
                   (union (list (car ls)) (get-foo* (cdr ls)))]
                  [(and (pair? (car ls)) (not (brick? (car ls))))
                   (union (get-foo* (car ls)) (get-foo* (cdr ls)))]
                  [else (get-foo* (cdr ls))]))])
      get-foo*)))

(test 'get-symbols-from-rules*-1
  (let ([R `((,GenMinor -> ,GenMinor_var1)
             (,GenMinor_var1 -> Cm)
             (,GenMinor_var2 -> Cm7)
             (,GenDom -> ,GenDom_var1)
             (,GenDom_var1 -> G7)
             (,GenDom_var2 -> ,GenII G7))])
    (get-symbols-from-rules* R))
  '(Cm Cm7 G7))

(test 'get-bricks-from-rules*-1
  (let ([R `((,GenMinor -> ,GenMinor_var1)
             (,GenMinor_var1 -> Cm)
             (,GenMinor_var2 -> Cm7)
             (,GenDom -> ,GenDom_var1)
             (,GenDom_var1 -> G7)
             (,GenDom_var2 -> ,GenII G7))])
    (get-bricks-from-rules* R))
  `(,GenMinor ,GenMinor_var1 ,GenMinor_var2 ,GenDom ,GenDom_var1 ,GenDom_var2 ,GenII))

(define new-hotness-cfg
  (let ([R `((,GenMinor -> ,GenMinor_var1)
             (,GenMinor_var1 -> Cm)
             (,GenMinor_var2 -> Cm7)
             (,GenDom -> ,GenDom_var1)
             (,GenDom_var1 -> G7)
             (,GenDom_var2 -> ,GenII G7))])
    (let ([dummy-start 'S])
      (let ([V (union (list dummy-start) (get-bricks-from-rules* R))]
            [Sigma (get-symbols-from-rules* R)])
        (make-cfg ,V ,Sigma ,R ,dummy-start)))))

(test 'normalize-new-hotness-cfg-1
  (prettify-object (normalize new-hotness-cfg))
  '(cfg-tag
    (U.4 S0 S GenMinor GenMinor_var1 GenMinor_var2 GenDom
         GenDom_var1 GenDom_var2 GenII)
    (Cm Cm7 G7)
    ((GenDom_var2 -> GenII U.4) (U.4 -> G7) (GenDom_var1 -> G7) (GenDom -> G7)
     (GenMinor_var2 -> Cm7) (GenMinor_var1 -> Cm)
     (GenMinor -> Cm))
    S0))


;;; Test Grammars
(define G3
  (let ([R '((S -> a S b)
             (S -> S S)
             (S -> epsilon))])
    (make-cfg (S) (a b) ,R S)))

(define G4
  (let ([R (convert-rules ((EXPR -> (EXPR + TERM) (TERM))
                           (TERM -> (TERM x FACTOR) (FACTOR))
                           (FACTOR -> (< EXPR >) (a))))])
    (make-cfg (EXPR TERM FACTOR) (a + x < >) ,R EXPR)))

(define G6
  (let ([R '((S -> A S A)
             (S -> a B)
             (A -> B)
             (A -> S)
             (B -> b)
             (B -> epsilon))])
    (make-cfg (S A B) (a b) ,R S)))

;;; add-start tests
(test 'add-start-1
  (add-start G3)
  (make-cfg (S0 S)
            (a b)
            ((S0 -> S)
             (S -> a S b)
             (S -> S S)
             (S -> epsilon))
            S0))

(test 'add-start-2
  (add-start G6)
  (make-cfg (S0 S A B)
            (a b)
            ((S0 -> S)
             (S -> A S A)
             (S -> a B)
             (A -> B)
             (A -> S)
             (B -> b)
             (B -> epsilon))
            S0))

;;; remove-first-non-start-epsilon-rule tests
(test 'remove-first-non-start-epsilon-rule-1
  (let ([R '((S0 -> S)
             (S -> A S A)
             (S -> a B)
             (A -> B)
             (A -> S)
             (B -> b)
             (B -> epsilon))]
        [S 'S0])
    (remove-first-non-start-epsilon-rule R S))
  '(((S0 -> S)
     (S -> A S A)
     (S -> a B)
     (A -> B)
     (A -> S)
     (B -> b))
    B))

(test 'remove-first-non-start-epsilon-rule-2
  (let ([R '((S0 -> S)
             (S -> A S A)
             (S -> a B)
             (S -> a)
             (A -> B)
             (A -> S)
             (A -> epsilon)
             (B -> b))]
        [S 'S0])
    (remove-first-non-start-epsilon-rule R S))
  '(((S0 -> S)
     (S -> A S A)
     (S -> a B)
     (S -> a)
     (A -> B)
     (A -> S)
     (B -> b))
    A))

(test 'remove-first-non-start-epsilon-rule-3
  (let ([R '((S0 -> S)
             (S0 -> epsilon)
             (S -> A S A)
             (S -> a B)
             (S -> a)
             (A -> B)
             (A -> S)
             (A -> epsilon)
             (B -> b))]
        [S 'S0])
    (remove-first-non-start-epsilon-rule R S))
  '(((S0 -> S)
     (S0 -> epsilon)
     (S -> A S A)
     (S -> a B)
     (S -> a)
     (A -> B)
     (A -> S)
     (B -> b))
    A))

(test 'remove-first-non-start-epsilon-rule-4
  (let ([R '((S0 -> S)
             (S0 -> epsilon)
             (S -> A S A)
             (S -> a B)
             (S -> a)
             (A -> B)
             (A -> S)
             (B -> b))]
        [S 'S0])
    (remove-first-non-start-epsilon-rule R S))
  '(((S0 -> S)
     (S0 -> epsilon)
     (S -> A S A)
     (S -> a B)
     (S -> a)
     (A -> B)
     (A -> S)
     (B -> b))
    #f))

;;; generate-rules tests
(test 'generate-rules-1
  (generate-rules 'A 'R '(u A v A w))
  '((R -> u v w)
    (R -> u A v w)
    (R -> u v A w)
    (R -> u A v A w)))

(test 'generate-rules-2
  (generate-rules 'A 'S '(A S A))
  '((S -> S)
    (S -> A S)
    (S -> S A)
    (S -> A S A)))

(test 'generate-rules-3
  (generate-rules 'A 'D '(A))
  '((D -> A)))

(test 'generate-rules-4
  (generate-rules 'A 'B '(A A))
      '((B -> A)
        (B -> A)
        (B -> A A)))

;;; convert-rules tests
(test 'convert-rules
  (convert-rules
   ((A -> (B) (epsilon) (B C))
    (B -> (epsilon) (C D E))))
  '((B -> epsilon)
    (B -> C D E)
    (A -> B)
    (A -> epsilon)
    (A -> B C)))

;;; add-rules tests
(test 'add-rules-1
  (add-rules '((Z -> w)
               (R -> u A v A w)
               (B -> C)
               (D -> E v F))
             'A
             '(A))
  '((Z -> w)
    (R -> u A v A w)
    (R -> u v A w)
    (R -> u A v w)
    (R -> u v w)
    (B -> C)
    (D -> E v F)))

(test 'add-rules-2
  (add-rules '((Z -> w)
               (R -> u A v A w)
               (B -> C)
               (D -> E v F))
             'A
             '(C A F))
  '((Z -> w)
    (R -> u A v A w)
    (R -> u v A w)
    (R -> u A v w)
    (R -> u v w)
    (B -> C)
    (D -> E v F)))

(test 'add-rules-3
  (add-rules '((Z -> w)
               (R -> u A v A w)
               (B -> C)
               (R -> A)
               (D -> E v F))
             'A
             '(A))
  '((Z -> w)
    (R -> u A v A w)
    (R -> u v A w)
    (R -> u A v w)
    (R -> u v w)
    (B -> C)
    (R -> A)
    (R -> epsilon)
    (D -> E v F)))

(test 'add-rules-4
  (add-rules '((Z -> w)
               (R -> u A v A w)
               (B -> C)
               (R -> A)
               (D -> E v F))
             'A
             '(A R))
  '((Z -> w)
    (R -> u A v A w)
    (R -> u v A w)
    (R -> u A v w)
    (R -> u v w)
    (B -> C)
    (R -> A)
    (D -> E v F)))

(test 'add-rules-5
  (add-rules '((S0 -> S)
               (S -> S S)
               (S -> a))
             'S
             '(S))
  '((S0 -> S)
    (S0 -> epsilon)
                (S -> S S)
                (S -> S)
                (S -> a)))

(test 'add-rules-6
  (add-rules '((S0 -> S)
               (S -> A S A)
               (S -> a B)
               (A -> B)
               (A -> S)
               (B -> b))
             'B
             '(B))
  '((S0 -> S)
    (S -> A S A)
    (S -> a B)
    (S -> a)
    (A -> B)
    (A -> epsilon)
    (A -> S)
    (B -> b)))

(test 'add-rules-7
  (add-rules  '((S0 -> S)
                (S -> A S A)
                (S -> a B)
                (S -> a)
                (A -> B)
                (A -> S)
                (B -> b))
              'A
              '(A B))
  '((S0 -> S)
    (S -> A S A)
    (S -> S A)
    (S -> A S)
    (S -> S)
    (S -> a B)
    (S -> a)
    (A -> B)
    (A -> S)
    (B -> b)))

(test 'add-rules-8
  (add-rules '((S0 -> S)
               (S -> A)
               (A -> B)
               (B -> A)
               (B -> S A))
             'A
             '(A))
  '((S0 -> S)
    (S -> A)
    (S -> epsilon)
    (A -> B)
    (B -> A)
    (B -> epsilon)
    (B -> S A)
    (B -> S)))

(test 'add-rules-9
  (add-rules '((S0 -> S)
               (S -> A)
               (A -> B)
               (B -> A)
               (B -> epsilon)
               (B -> S A)
               (B -> S))
             'S
             '(S A))
  '((S0 -> S)
    (S0 -> epsilon)
    (S -> A)
    (A -> B)
    (B -> A)
    (B -> epsilon)
    (B -> S A)
    (B -> S)))

(test 'add-rules-10
  (add-rules  '((S0 -> S)
                (S0 -> epsilon)
                (S -> A)
                (A -> B)
                (B -> A)
                (B -> S A)
                (B -> S))
              'B
              '(B S A))
  '((S0 -> S)
    (S0 -> epsilon)
    (S -> A)
    (A -> B)
    (B -> A)
    (B -> S A)
    (B -> S)))

(test 'add-rules-11
  (add-rules '((S0 -> S)
               (S -> S S)
               (S -> S S S)
               (S -> a))
             'S
             '(S))
  '((S0 -> S)
    (S0 -> epsilon)
    (S -> S S)
    (S -> S)
    (S -> S S S)
    (S -> a)))

(test 'add-rules-12
  (add-rules '((S0 -> S)
               (S -> A)
               (S -> epsilon)
               (A -> B)
               (B -> A)
               (B -> epsilon)
               (B -> S A)
               (B -> S))
             'S
             '(S A))
  '((S0 -> S)
    (S0 -> epsilon)
    (S -> A)
    (S -> epsilon)
    (A -> B)
    (B -> A)
    (B -> epsilon)
    (B -> S A)
    (B -> S)))

(test 'add-rules-13
  (add-rules '((S0 -> S)
               (S -> A)
               (S -> epsilon)
               (S -> epsilon)
               (A -> B)
               (B -> A)
               (B -> epsilon)
               (B -> S A)
               (B -> S))
             'S
             '(S A))
  '((S0 -> S)
    (S0 -> epsilon)
    (S -> A)
    (S -> epsilon)
    (A -> B)
    (B -> A)
    (B -> epsilon)
    (B -> S A)
    (B -> S)))

;;; remove-first-unit-rule tests
(test 'remove-first-unit-rule-1
  (remove-first-unit-rule
   '((Z -> w)
     (R -> u A v A w)
     (R -> u v A w)
     (R -> u A v w)
     (R -> u v w)
     (B -> C)
     (R -> A)
     (D -> E v F))
   '(A B C D E F R Z))
      '(((Z -> w)
         (R -> u A v A w)
         (R -> u v A w)
         (R -> u A v w)
         (R -> u v w)
         (R -> A)
         (D -> E v F))
        (B -> C)))

;;; substitute-unit-rule tests
(test 'substitute-unit-rule-1
  (substitute-unit-rule
   '((Z -> w)
     (R -> u A v A w)
     (R -> u v A w)
     (R -> u A v w)
     (R -> u v w)
     (R -> A)
     (C -> E v F))
   '(B -> C)
   '())
  '((Z -> w)
    (R -> u A v A w)
    (R -> u v A w)
    (R -> u A v w)
    (R -> u v w)
    (R -> A)
    (B -> E v F)
    (C -> E v F)))

;;; handle-unit-rules
(test 'handle-unit-rules-1
  (handle-unit-rules
   (make-cfg
    (S0 S A B)
    (u b c)
    ((S0 -> S)
     (S -> A)
     (A -> u B b)
     (B -> c))
    S0))
  (make-cfg
   (S0 S A B)
   (u b c)
   ((S0 -> u B b)
    (S -> u B b)
    (A -> u B b)
    (B -> c))
   S0))

(test 'handle-mixed-rules-1
  (handle-mixed-rules
   (make-cfg
    (A B E)
    (a c d)
    ((A -> a B c d E))
    A))
  (make-cfg
   (U.3 A.3 U.2 A.2 A.1 U.1 A B E)
   (a c d)
   ((A.3 -> U.3 E)
    (U.3 -> d)
    (A.2 -> U.2 A.3)
    (U.2 -> c)
    (A.1 -> B A.2)
    (A -> U.1 A.1)
    (U.1 -> a))
   A))

(test 'normalize-G6
  (normalize G6)
  (make-cfg
   (U.3 U.2 U.1 A.3 A.2 A.1 S0 S A B)
   (a b)
   ((B -> b)
    (A -> b)
    (S -> a)
    (A -> a)
    (S0 -> a)
    (S -> U.3 B)
    (U.3 -> a)
    (A -> U.2 B)
    (U.2 -> a)
    (S0 -> U.1 B)
    (U.1 -> a)
    (S -> A S)
    (A -> A S)
    (S0 -> A S)
    (S -> S A)
    (A -> S A)
    (S0 -> S A)
    (A.3 -> S A)
    (S -> A A.3)
    (A.2 -> S A)
    (A -> A A.2)
    (A.1 -> S A)
    (S0 -> A A.1))
   S0))

(test 'normalize-two-constant-rule
  (let ([G (make-cfg
            (A)
            (c d)
            ((A -> c d))
            A)])
    (normalize G))
  (make-cfg
   (U.2 U.3 U.0 U.1 S0 A)
   (c d)
   ((A -> U.2 U.3)
    (U.2 -> c)
    (U.3 -> d)
    (S0 -> U.0 U.1)
    (U.0 -> c)
    (U.1 -> d))
   S0))

(test 'normalize-M7sus4-to-3
  (let ([G (make-cfg
            (S M7sus4-to-3)
            (Bb7sus4 Bb7)
            ((S -> M7sus4-to-3)
             (M7sus4-to-3 -> Bb7sus4 Bb7))
            S)])
    (normalize G))
  (make-cfg
    (U.4 U.5 U.2 U.3 U.0 U.1 S0 S M7sus4-to-3)
    (Bb7sus4 Bb7)
    ((M7sus4-to-3 -> U.4 U.5)
     (U.4 -> Bb7sus4)
     (U.5 -> Bb7)
     (S -> U.2 U.3)
     (U.2 -> Bb7sus4)
     (U.3 -> Bb7)
     (S0 -> U.0 U.1)
     (U.0 -> Bb7sus4)
     (U.1 -> Bb7))
    S0))

(define l1a '((Autumn-Leaves-Opening_withoverrun -> (Straight-Approach -> (U.1832 -> Dm7) (U.1833 -> G7)) (A.229 -> (U.232 -> C) (A.231 -> (U.233 -> F) (Sad-Cadence -> (Sad-Approach -> (U.1714 -> Dm7b5) (U.1715 -> G7)) (U.1719 -> Cm)))))))

(define l1b '((Autumn-Leaves-Opening_withoverrun -> (Straight-Approach -> (U.1832 -> Dm7) (U.1833 -> G7)) (A.229 -> (U.237 -> C) (A.231 -> (U.233 -> F) (Sad-Cadence -> (Sad-Approach -> (U.1714 -> Dm7b5) (U.1715 -> G7)) (U.1719 -> Cm)))))))

;;; not apha equivalent to l1a and l1b
(define l2 '((Autumn-Leaves-Opening_withoverrun -> (Straight-Approach -> (U.1832 -> Dm7) (U.1833 -> G7)) (A.229 -> (U.233 -> C) (A.231 -> (U.233 -> F) (Sad-Cadence -> (Sad-Approach -> (U.1714 -> Dm7b5) (U.1715 -> G7)) (U.1719 -> Cm)))))))

(define l3 '((Autumn-Leaves-Opening_withoverrun -> (Straight-Approach -> (U.1832 -> Dm7) (U.1833 -> G7)) (A.229 -> (U.233 -> D) (A.231 -> (U.233 -> F) (Sad-Cadence -> (Sad-Approach -> (U.1714 -> Dm7b5) (U.1715 -> G7)) (U.1719 -> Cm)))))))

(test 'equal-modulo-genny-1
  (and (equal-modulo-genny? l1a l1b) #t)
  #t)

(test 'equal-modulo-genny-2
  (and (equal-modulo-genny? l1a l2) #t)
#f)

(test 'equal-modulo-genny-3
  (and (equal-modulo-genny? '(U.1 U.2) '(U.3 U.4)) #t)
  #t)

(test 'equal-modulo-genny-4
  (and (equal-modulo-genny? '(U.1 U.2) '(U.3 U.3)) #t)
  #f)

(test 'equal-modulo-genny-5
  (and (equal-modulo-genny? l1a l3) #t)
  #f)

(test 'equal-modulo-genny-6
  (and (equal-modulo-genny? '(U.3 U.3) '(U.1 U.2)) #t)
  #f)

