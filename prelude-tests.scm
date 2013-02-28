(load "prelude.scm")

(test 'contains-dups?-1
  (contains-dups? '(a b c))
  #f)

(test 'contains-dups?-2
  (contains-dups? '(a b c a))
  #t)
