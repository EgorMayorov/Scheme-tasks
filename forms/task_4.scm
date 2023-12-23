#lang racket

(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))
(define (task-4 tree h)
  (cond ((and (empty-tree? tree) (= h 0)) #t)
        ((and (empty-tree? tree) (not (= h 0))) #f)
        (else (and (or (task-4 (tree-left tree) (- h 1)) (task-4 (tree-left tree) (- h 2))) (or (task-4 (tree-right tree) (- h 1)) (task-4 (tree-right tree) (- h 2)))))
  )
)

(task-4 #() 0) ; #t
(task-4 #(1 #() #()) 1) ; #t
(task-4 #() 1) ; #f
(task-4 #(10 #(21 #(31 #() #()) #()) #(22 #() #(34 #() #()))) 3) ; #t