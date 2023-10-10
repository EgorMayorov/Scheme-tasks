#lang scheme/base
(require math/number-theory)

; I.
(define (vector-fold-right func init-val vctr)
  (let ((length (vector-length vctr)))
    ; создается итеративный цикл для прохода по всему вектору
    (let loop ((i (- length 1)) (result init-val))
      (if (= i -1)
          result
          (loop (- i 1) (func i result (vector-ref vctr i)))
      )
    )
  )
)

(println 'Task_1_examples)
(vector-fold-right (lambda (i val el) (cons el val)) '() #()) ; -> '()
(vector-fold-right (lambda (i val el) (cons el val)) '() #(10 20 30)) ; -> '(10 20 30)
(newline)



; II.
(define (fun2a n)
  (reverse
   ; создает рекурсивный процесс перебора всех чисел, и в случае, если число подходит - добавляет его в список и идет дальше, если не подходит - просто идет дальше
   (let loop ((x 2))
     (if (> x n)
         '()
         (if (and (divides? x n) (not (prime? x)))
             (cons x (loop (+ x 1)))
             (loop (+ x 1))
         )
     )
   )
  )
)


(define (fun2b n)
  ; итеративный процесс. В случае, если число подходит - добавляет его в список и идет дальше, либо просто идет дальше
  (let loop ((x 2) (result '()))
    (if (> x n)
        result
        (if (and (divides? x n) (not (prime? x)))
            (loop (+ x 1) (cons x result))
            (loop (+ x 1) result)
        )
    )
  )
)

(println 'Task_2_examples)
(fun2b 1) ; -> '()
(fun2a 2) ; -> '()
(fun2b 12) ; -> '(12 6 4)
(fun2a 12) ; -> '(12 6 4)
(newline)



; III.
(define (fun3 n)
  ; проход по числам и проверка их на простоту. Если число простое, то на него умножается результат, изначально равный 1
  (let loop ((x 1) (n n) (result 1))
    (if (= n 0)
        result
        (if (prime? x)
            (loop (+ x 1) (- n 1) (* result x))
            (loop (+ x 1) n result)
        )
    )
  )
)

(println 'Task_3_examples)
(fun3 0) ; -> 1
(fun3 4) ; -> 210
(fun3 10) ; -> 6469693230
(newline)



; IV.
(define (fun4 tree r1 r2)
  (let ((get-value (lambda (tree) (vector-ref tree 0))) ; корень дерева
        (left-tree (lambda (tree) (vector-ref tree 1))) ; левое поддерево
        (right-tree (lambda (tree) (vector-ref tree 2))) ; правое поддерево
       )
    ; проверка корня дерева на отрицательность и расстояние до корня начального дерева, и рекурсивый переход к обоим поддеревьям
    (let loop ((tree tree) (r-min (min r1 r2)) (r-max (max r1 r2)) (r 0))
      (if (equal? tree #())
          0
          (if (and
               (negative? (get-value tree))
               (>= r r-min)
               (<= r r-max)
              )
              (+ 1 (loop (left-tree tree) r-min r-max (+ r 1)) (loop (right-tree tree) r-min r-max (+ r 1))) 
              (+ 0 (loop (left-tree tree) r-min r-max (+ r 1)) (loop (right-tree tree) r-min r-max (+ r 1)))
          )
      )
    )
  )
)

(println 'Task_4_examples)
(fun4 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())) 2 1) ; -> 2
(fun4 #() 1 10) ; -> 0
(fun4 #(-10 #() #()) 0 0) ; -> 1
(newline)



; V.
(define (fun5 tree h)
  (let ((get-value (lambda (tree) (vector-ref tree 0))) ; корень дерева
        (left-tree (lambda (tree) (vector-ref tree 1))) ; левое поддерево
        (right-tree (lambda (tree) (vector-ref tree 2))) ; правое поддерево
        (empty-tree? (lambda (tree) (equal? tree #()))) ; проверка на пустоту дерева
       )
    (call/cc
     (lambda (cc-exit)
       (cond 
         ((= h 0) (if (empty-tree? tree)
                      #t
                      (cc-exit #f)
                  )
         )
         ((= h 1) (if (and
                       (not (empty-tree? tree))
                       (empty-tree? (left-tree tree))
                       (empty-tree? (right-tree tree))
                      )
                      #t
                      (cc-exit #f)
                  )
         )
         ((> h 1) (if (and
                       (fun5 (left-tree tree) (- h 1))
                       (or
                        (fun5 (right-tree tree) (- h 2))
                        (fun5 (right-tree tree) (- h 1))
                       )
                       (and
                        (> (get-value tree) (get-value (left-tree tree)))
                        (< (get-value tree) (get-value (right-tree tree)))
                       )
                      )
                      #t
                      (cc-exit #f)
                  )
         )
       )
     )
    )
  )
)

(println 'Task_5_examples)
(fun5 #() 0) ; -> #t
(fun5 #(10 #() #()) 1) ; -> #t
(fun5 #(1 #(-2 #() #()) #(3 #(2 #() #()) #(4 #() #()))) 3) ; -> #f
(fun5 #(1 #(-1 #(-3 #() #()) #(0 #() #())) #(2 #() #())) 3) ; -> #t