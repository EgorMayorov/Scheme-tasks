#lang racket

(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (print-tree-by-level-desc tree)
  (let list-print ((branch tree))
    (if (not (empty-tree? branch))
        (let ()
          (list-print (tree-left branch))
          (list-print (tree-right branch))
          (println (tree-data branch))
        )
        (void)
    )
  )
)

(print-tree-by-level-desc #())
(print-tree-by-level-desc #(1 #() #()))
(print-tree-by-level-desc #(10 #(21 #() #()) #(22 #() #())))
(print-tree-by-level-desc #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #()))))