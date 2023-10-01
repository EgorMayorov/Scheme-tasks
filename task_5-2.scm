#lang racket

(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))
(define (task-5 tree h)
  (call/cc (lambda (cc-exit)
             (let loop ((tree tree) (h h)) 
               (cond 
                 ((= h 0) (if (empty-tree? tree) #t (cc-exit #f)))
                 ((= h 1) (if (and (not (empty-tree? tree)) (empty-tree? (tree-left tree)) (empty-tree? (tree-right tree))) #t (cc-exit #f)))
                 ((> h 1) (if (and (task-5 (tree-left tree) (- h 1)) (task-5 (tree-right tree) (- h 2)))
                              #t
                              (cc-exit #f)
                          )
                 )
               )
             )
           )
  )
)

(task-5 #() 0)
(task-5 #(1 #() #()) 1)
(task-5 #() 1)
(task-5 #(10 #(21 #(35 #() #()) #()) #(22 #() #())) 3) 