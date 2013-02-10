(load "pmatch.scm")
(load "prelude.scm")

;;; Changes: (7/3/12)
;;; 1) replaced multi-rembero with split and rewrote generate-rules so that
;;;    minikanren is no longer necessary.
;;; 2) moved the Sipser grammars and tests into a separate file :
;;;   (cfgnorm-tests.scm).
;;; 3) added various comments.
;;; 4) added a draft for step 4: handle-mixed-units.
;;; 5) added new tests for add-rules.

(define genny
  (let ((n 0))
    (lambda (v)
      (case v
        ((reset) (set! n 0))
        (else
         (let ((g (string->symbol
                   (string-append (symbol->string v) "." (number->string n)))))
           (set! n (add1 n))
           g))))))

(define genny?
  (lambda (x)
    (and (symbol? x)
         (memv #\. (string->list (symbol->string x)))
         #t)))

;;; fix me: needs to respect alpha equivalence of gensyms (when
;;; duplicates appear in same expression)
(define equal-modulo-genny?
  (lambda (ge e)
    (and (equal-modulo-genny?-aux ge e '())
         (equal-modulo-genny?-aux e ge '()))))

(define equal-modulo-genny?-aux
  (lambda (ge e env)
    (and
      env
      (cond
        [(genny? ge)
         (cond
           ((assq ge env) => (lambda (x) (and (eq? (cdr x) e) env)))
           (else
            (and (symbol? e) (cons (cons ge e) env))))]
        ((and (pair? e) (pair? ge))
         (equal-modulo-genny?-aux (cdr ge) (cdr e)
                                  (equal-modulo-genny?-aux (car ge) (car e) env)))
        (else (and (equal? e ge) env))))))

(define-syntax test
  (syntax-rules ()
    [(_ name e1 e2)
     (begin
       (genny 'reset)
       (let ([v1 e1][v2 e2])
         (if (equal-modulo-genny? v1 v2)
             (printf "Test ~s passed. Success! (Wiggle eyebrows.)\n" name)
             (begin
               (printf "\nTest ~s failed.\n\n~s\n=>\n~s\n\n~s\n=>\n~s\n" name 'e1 v1 'e2 v2)
               (errorf 'test "\nTest ~s failed!" name)))))]))

(define non-terminal?
  (lambda (x)
    (or (symbol? x)
        (brick? x))))

(define terminal?
  (lambda (x)
    (symbol? x)))


;;; Similar to pmatch, except exclusively for cfg tagged lists.
(define-syntax cfg-match
  (syntax-rules (cfg-tag)
    [(_ e [(V Sigma R S) b b* ...])
     (let ([v e])
       (pmatch v
         [(cfg-tag V Sigma R S) b b* ...]
         [else (errorf 'cfg-match "~s => ~s did not match pattern ~s\n"
                       'e v '(V Sigma R S))]))]))

;;; Macros -------------------------------------------------------------------
;;; Takes a V (list of variables), Sigma (list of terminals), R (list of rules),
;;; and S (start symbol) and creates a cfg tagged list.
;;; (make-cfg ,V ,Sigma ,R ,S) => `(cfg-tag ,V ,Sigma ,R ,S)
(define-syntax make-cfg
  (syntax-rules ()
    [(_ v sigma r s)
     (let ([V `v][Sigma `sigma][R `r][S `s])
       (assert (non-terminal? S))
       (assert (element? S V))
       (assert (andmap non-terminal? V))
       (assert (andmap terminal? Sigma))
       (assert (not (contains-dups? V)))
       (assert (not (contains-dups? Sigma)))
       (assert (disjoint? V Sigma))
       (assert (verify-cfg-rules V Sigma R S))       
       (list 'cfg-tag V Sigma R S))]))

(define verify-cfg-rules
  (lambda (V Sigma R S)
    (let ([V+Sigma (union V Sigma)])
      (let loop ([R R])
        (pmatch R "verify-cfg-rules"
          [() 'success!]
          [((,A -> ,B . ,B*) . ,rest) (guard
                                       (memq A V)
                                       (memq B V+Sigma)
                                       (andmap (lambda (B) (memq B V+Sigma)) B*))
           (loop rest)]
          [((,A -> epsilon) . ,rest)
           (loop rest)]                    
          [else (errorf 'verify-cfg-rules "Rule ~s is not a valid CFG rule" (car R))])))))

;;; Takes a list of rules of the form ((A -> B B* ...) ...)  where B, B*, etc.
;;; are lists, and returns an equivalent list of rules in the form ((A -> B),
;;; (A -> B*) ...)
(define-syntax convert-rules
  (syntax-rules (->)
    [(_ ((A -> r r* ...)
         (As -> rs rs* ...)
         ...))
     (convert-rules-aux
      ((A -> r r* ...)
       (As -> rs rs* ...)
       ...)
      ())]))

;;; A recursive helper for convert-rules.
(define-syntax convert-rules-aux
  (syntax-rules (->)
    [(_ () acc) 'acc]
    [(_ ((A -> (B B* ...) (Cs Cs* ...) ...) cs ...) acc)
     (convert-rules-aux
      (cs ...)
      ((A -> B B* ...) (A -> Cs Cs* ...) ... . acc))]))
;;; ------------------------------------------------------------------------

;;; Step 1 - Add a new start symbol S0. ------------------------------------
(define add-start
  (lambda (cfg)
    (cfg-match cfg
      [(,V ,Sigma ,R ,S)
       (let ([new-S 'S0])
         (let ([V `(,new-S . ,V)]
               [R `((,new-S -> ,S) . ,R)])
           (make-cfg ,V ,Sigma ,R ,new-S)))])))
;;; ------------------------------------------------------------------------

;;; Step 2 - remove all rules of the form (A -> epsilon). ------------------
(define handle-epsilons
  (lambda (cfg)
    (cfg-match cfg
      [(,V ,Sigma ,R ,S)
       (let loop ([R/A (remove-first-non-start-epsilon-rule R S)]
                  [previously-removed '()])
         (pmatch R/A "handle-epsilons"
                 [(,R #f) (make-cfg ,V ,Sigma ,R ,S)]
                 [(,R ,A)
                  (let ([previously-removed (cons A previously-removed)])
                    (let ([R (add-rules R A previously-removed)])
                      (loop (remove-first-non-start-epsilon-rule R S)
                            previously-removed)))]))])))

;;; Removes the first rule of the form (A -> epsilon)
;;; from the list of rules R, provided that A is not the start symbol S.
;;; Returns a list of two values (R^ A/#f), where r^ is the new list of rules
;;; (possibly unchanged), and A/#f is either A (if a rule was removed)
;;; or #f (if no rule was removed).
(define remove-first-non-start-epsilon-rule
  (lambda (R S)
    (let loop ([R R][R^ '()])
      (pmatch R "r-f-n-s-e-r"
        [() (list (reverse R^) #f)]
        [((,A -> epsilon) . ,rest) (guard (not (eqv? A S)))
         (list (append (reverse R^) rest) A)]
        [((,A -> . ,a*) . ,rest)
         (loop rest (cons `(,A -> . ,a*) R^))]))))

;;; Takes a list of rules R, a variable symbol A, and a list of variables
;;; previously removed. Assumes that a rule (A -> epsilon) has previously
;;; been removed from R, and that A is a member of previously-removed.
;;; Adds new rules to compensate for any right-hand occurrences of A.
;;; (B -> A S A) => (B -> A S A), (B -> A S), (B -> S A), (B -> S)
(define add-rules
  (lambda (R A previously-removed)
    (let loop ([R R][R^ '()])
      (pmatch R "add-rules"
        [() (reverse (remove-dups R^))]
        [((,B -> ,C) . ,rest) (guard (eqv? A C))
         (let ([R^ (cons `(,B -> ,C) R^)])
           (if (memv B previously-removed)
               (loop rest R^)
               (loop rest (cons `(,B -> epsilon) R^))))]
        [((,B -> . ,C*) . ,rest)
         (if (memv A C*)
             (loop rest (append (remove-dups (generate-rules A B C*)) R^))
             (loop rest (cons `(,B -> . ,C*) R^)))]))))

;;; Takes variable symbols A and B and a list of variables rule, such that
;;; `(,B -> . ,rule) and A is a member of rule. Returns a list of rules
;;; consisting on additional rules that need to be added to compensate for
;;; A being represented by epsilon.
(define generate-rules
  (lambda (A B rule)
    (let loop ([split-ls (split A rule '(()))]
               [rules '()])
      (cond
        [(null? split-ls) (reverse rules)] ;; remove reverse later
        [(null? (car split-ls)) (loop (cdr split-ls) rules)]
        [else (loop (cdr split-ls)
                    (cons `(,B -> . ,(car split-ls)) rules))]))))

;;; A helper to generate-rules. Takes val (A), ls (rule), and ans (which must
;;; initially be '(()). Returns a list of lists containing all possible
;;; enumerations of rule, assuming that A can be represented as epsilon.
(define split
  (lambda (val ls ans)
    (cond
      [(null? ls) ans]
      [else (let ([rest (map (lambda (l) (append l (list (car ls)))) ans)])
              (if (eq? (car ls) val)
                  (split val (cdr ls) (append ans rest))
                  (split val (cdr ls) rest)))])))
;;; --------------------------------------------------------------------------


;;; Step 3 - removes all rules of the form (A -> B). --------------------------
(define handle-unit-rules
  (lambda (cfg)
    (cfg-match cfg
      [(,V ,Sigma ,R ,S)
       (let loop ([R/A (remove-first-unit-rule R V)]
                  [prev-removed '()])
         (pmatch R/A "handle unit rules"
                 [(,R #f) (let ([R (remove-dups R)])
                            (make-cfg ,V ,Sigma ,R ,S))]
                 [(,R ,r)
                  (let ([R (substitute-unit-rule R r prev-removed)])
                    (loop (remove-first-unit-rule R V)
                          (cons r prev-removed)))]))])))

(define remove-first-unit-rule
  (lambda (R V)
    (let loop ([R R][R^ '()])
      (pmatch R "r-f-u-r"
              [() (list (reverse R^) #f)]
              [((,A -> ,B) . ,rest) (guard (member B V))
               (list (append (reverse R^) rest) `(,A -> ,B))]
              [((,A -> . ,any*) . ,rest)
               (loop rest (cons `(,A -> . ,any*) R^))]))))

(define substitute-unit-rule
  (lambda (R r prev-removed)
    (pmatch r "s-u-r"
      [(,A -> ,B)
       (let loop ([R R][R^ '()])
         (pmatch R "sub unit rule"
           [() (reverse R^)]
           [((,C -> . ,D*) . ,rest) (guard (eqv? B C))
            (let ([new-rule `(,A -> . ,D*)])
              (if (member new-rule prev-removed)
                  (loop rest (cons `(,C -> . ,D*) R^))
                  (loop rest (cons `(,C -> . ,D*) (cons new-rule R^)))))]
           [((,C -> . ,D*) . ,rest)
            (loop rest (cons `(,C -> . ,D*) R^))]))])))
;;; ------------------------------------------------------------------------

;;; Step 4 - Remove rules with right sides consisting of both variables and
;;; terminal symbols. ------------------------------------------------------
(define handle-mixed-rules
  (lambda (cfg)
    (cfg-match cfg
      [(,V ,Sigma ,R ,S)
       (let loop ([old-R R]
                  [ans '()]
                  [V V])
         (pmatch old-R
           [() (make-cfg ,V ,Sigma ,ans ,S)]
           [((,A -> ,B) . ,rest) (loop rest (cons `(,A -> ,B) ans) V)]
           [((,A -> . ,B*) . ,rest)
            (if (>= (length B*) 2)
                (pmatch (handle-mixed-rule `(,A -> . ,B*) Sigma V)
                  [(,ans^ ,V) (loop rest (append ans^ ans) V)])
                (loop rest (cons `(,A -> . ,B*) ans) V))]))])))

(define handle-mixed-rule
  (lambda (rule Sigma V)
    (let loop ([rule rule]
               [ans '()]
               [V V])
      (pmatch rule
        [(,A -> ,u1) `(,ans ,V)]        
        [(,A -> ,v1 ,v2) (guard (memv v1 V) (memv v2 V))
         (let ([ans (cons `(,A -> ,v1 ,v2) ans)])
           `(,ans ,V))]
        [(,A -> ,c1 ,c2) (guard (memv c1 Sigma) (memv c2 Sigma))
         (let ([U1 (genny 'U)]
               [U2 (genny 'U)])
           (let ([V (cons U1 (cons U2 V))]
                 [ans (cons `(,A -> ,U1 ,U2)
                            (cons `(,U1 -> ,c1)
                                  (cons `(,U2 -> ,c2) ans)))])
             `(,ans ,V)))]
        [(,A -> ,u1 ,u2)
         (let ([U1 (genny 'U)])
           (if (memv u1 Sigma)
               (let ([V (cons U1 V)])
                 (let ([ans (cons `(,A -> ,U1 ,u2) (cons `(,U1 -> ,u1) ans))])
                   `(,ans ,V)))
               (let ([V (cons U1 V)])
                 (let ([ans (cons `(,A -> ,u1 ,U1) (cons `(,U1 -> ,u2) ans))])
                   `(,ans ,V)))))]
        [(,A -> ,u1 . ,u*)
         (let ([A1 (genny 'A)] [U1 (genny 'U)])
           (if (memv u1 Sigma)
               (let ([V (cons A1 (cons U1 V))])
                 (loop `(,A1 -> . ,u*)
                       (cons `(,A -> ,U1 ,A1) (cons `(,U1 -> ,u1) ans)) V))
               (let ([V (cons A1 V)])
                 (loop `(,A1 -> . ,u*)
                       (cons `(,A -> ,u1 ,A1) ans) V))))]))))

;;; Verify a cfg is in Chomsky normal form. ----------------------------------
(define verify-cnf
  (lambda (cfg)
    (cfg-match cfg
      [(,V ,Sigma ,R ,S)
       (let loop ([R R])
         (pmatch R "verify-cnf"
           [() 'success!]
           [((,A -> ,B ,C) . ,rest) (guard (memq A V) (memq B V) (memq C V))
            (loop rest)]
           [((,A -> ,alpha) . ,rest) (guard (memq A V) (memq alpha Sigma))
            (loop rest)]
           [((,A -> epsilon) . ,rest) (guard (eq? A S))
            (loop rest)]
           [else (errorf 'verify-cnf "Rule ~s is not in Chomsky Normal Form" (car R))]))])))

;;; --------------------------------------------------------------------------

;;; Takes a cfg and converts it to Chomsky normal form. ----------------------
(define normalize
  (lambda (cfg)
    (let ([step1 (add-start cfg)])
      (let ([step2 (handle-epsilons step1)])
        (let ([step3 (handle-unit-rules step2)])
          (let ([step4 (handle-mixed-rules step3)])
            step4))))))
;;;---------------------------------------------------------------------------
