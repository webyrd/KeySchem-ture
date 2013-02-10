;;; Commonly used helper functions and macros

(define-syntax test
  (syntax-rules ()
    [(_ name e1 e2)
     (let ([v1 e1][v2 e2])
       (if (equal? v1 v2)
           (printf "Test ~s passed. Success! (Wiggle eyebrows.)\n" name)
           (begin
            (printf "\nTest ~s failed.\n\n~s\n=>\n~s\n\n~s\n=>\n~s\n" name 'e1 v1 'e2 v2)
            (errorf 'test "\nTest ~s failed!" name))))]))

(define remv-dups
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [(memv (car ls) (cdr ls)) (remv-dups (cdr ls))]
      [else (cons (car ls) (remv-dups (cdr ls)))])))

(define remove-dups
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [(member (car ls) (cdr ls)) (remove-dups (cdr ls))]
      [else (cons (car ls) (remove-dups (cdr ls)))])))

(define contains-dups?
  (lambda (ls)
    (cond
      [(null? ls) #f]
      [(memv (car ls) (cdr ls))
       (printf " *** dup detected:  ~s\n" (car ls))
       #t]
      [else (contains-dups? (cdr ls))])))

;;; Set operations
(define empty-set
  (lambda ()
    '()))

(define empty-set?
  (lambda (s)
    (null? s)))

(define singleton
  (lambda (x)
    (list x)))

(define set->list
  (lambda (s)
    s))

(define list->set
  (lambda (lem)
    (remv-dups lem)))

(define element?
  (lambda (x s)
    (memv x s)))

(define union
  (lambda args
    (union* args)))

(define union*
  (lambda (s*)
    (cond
      [(null? s*) '()]
      [else (union2 (car s*) (union* (cdr s*)))])))

(define union2
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(memv (car s1) s2) (union2 (cdr s1) s2)]
      [else (cons (car s1) (union2 (cdr s1) s2))])))

(define intersection
  (lambda (s1 s2)
    (cond
      [(null? s1) '()]
      [(memv (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))]
      [else (intersection (cdr s1) s2)])))

(define difference
  (lambda (s1 s2)
    (cond
      [(null? s1) '()]
      [(memv (car s1) s2) (difference (cdr s1) s2)]
      [else (cons (car s1) (difference (cdr s1) s2))])))

(define disjoint?
  (lambda (s1 s2)
    (cond
      [(null? s1) #t]
      [(memv (car s1) s2) #f]
      [else (disjoint? (cdr s1) s2)])))

(define subset?
  (lambda (s1 s2)
    (cond
      [(null? s1) #t]
      [(memv (car s1) s2) (subset? (cdr s1) s2)]
      [else #f])))

(define set-equal?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(define qualified-name
  (lambda (name variant)
    (if (null? variant)
        name
        (string->symbol (string-append (symbol->string name) "_" (object->string variant))))))

;;; ? does this need to be updated to handle bricks?
(define object->string
  (lambda (x)
    (cond
      [(string? x) x]
      [(number? x) (number->string x)]
      [(symbol? x) (symbol->string x)]
      [(list? x) (apply string-append (map object->string x))])))

(define brick->brick-name
  (lambda (x)
    (unless (brick? x) (error 'brick->brick-name "not a brick"))
    (cadr x)))

(define prettify-object
  (lambda (x)
    (cond
      [(brick? x) (brick->brick-name x)]
      [(pair? x) (cons (prettify-object (car x))
                       (prettify-object (cdr x)))]
      [else x])))
