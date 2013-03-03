(load "cyk.scm")

(test 'make-chart-1
  (make-chart 2 3 4)
  '#(#(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f #f))
     #(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f #f))))

(test 'update-chart-1
  (let ((c (make-chart 2 3 4)))
    (update-chart! c 1 2 3 'success)
    c)
  '#(#(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f #f))
     #(#(#f #f #f #f) #(#f #f #f #f) #(#f #f #f success))))

(test 'lookup-chart-1
  (let ((c (make-chart 2 3 4)))
    (update-chart! c 1 2 3 'success)
    (lookup-chart c 1 2 3))
  'success)

(test 'lookup-chart-2
  (let ((c (make-chart 2 3 4)))
    (update-chart! c 1 2 3 'success)
    (lookup-chart c 1 0 3))
  #f)

(test 'var->index
  (var->index 'C '(A B C D E F G))
  3)
