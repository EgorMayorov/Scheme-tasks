#lang racket

(define (taskI lst)
    (let loop ((cur-min +inf.0) (counter 0) (lst lst) (result '()))
        (cond 
            ((null? lst) (reverse result))
            ((< (car lst) cur-min) (loop (car lst) (+ counter 1) (cdr lst) (list counter)))
            ((= (car lst) cur-min) (loop cur-min (+ counter 1) (cdr lst) (cons counter result)))
            (else (loop cur-min (+ counter 1) (cdr lst) result))
        )
    )
)

(println '(My tests for task 1))
(taskI (list -1 0 -2 3 -2 -1 0 -3 -2 -3 -1)) ; (7 9)
(taskI (list -1 0 1 -1 0 1 -1)) ; (0 3 6)
(taskI (list 832183)) ; (0)
(taskI '()) ; ()



(define (taskII t s)
    (foldl 
        (lambda (x val) 
            (if (number? x) (+ (* x (/ s 4)) val)
                (+ val (taskII x (/ s 4)))
            )
        )
        0
        (vector->list t)
    )
)

(println '(My tests for task 2))
(taskII #(1 0 0 #(1 1 1 0)) 16) ; 7
(taskII #(1 #(0 1 0 1) 0 #(1 1 1 0)) 16) ; 9



(define (taskIII t)
  (call/cc (lambda (cc-exit)
             (begin 
                (define dfs (lambda (t s)
                              (cond
                                ((equal? t 1) s)
                                ((equal? t 0) 0)
                                (else 
                                 (let ((s_quarter (/ s 4)) (s_half (/ s 2)))
                                   (foldl (lambda (x result)
                                            (let* ((sum (car result))
                                                   (quarters_left (cadr result))
                                                   (new_sum (+ (dfs x s_quarter) sum))
                                                   (max_sum (+ new_sum (* quarters_left s_quarter)))
                                                   (min_sum new_sum))
                                              (cond 
                                                ((<= max_sum s_half) (cc-exit #f))
                                                ((> min_sum s_half) (cc-exit #t))
                                                (else (list new_sum (- quarters_left 1)))
                                              )
                                            )
                                          )
                                          (list 0 3)
                                          (list (vector-ref t 0) (vector-ref t 1) (vector-ref t 2) (vector-ref t 3))
                                   )
                                 )
                                )
                              )
                            )
                )                
                (let ((dfs-result (dfs t 1)))
                  (cond
                    ((= dfs-result 1) #t)
                    ((= dfs-result 0) #f)
                    (else dfs-result)
                  )
                )
             )
           )
  )
)

(println '(My tests for task 3))
(taskIII #(1 1 0 #(1 1 1 0))) ; #t
(taskIII #(0 0 0 #(1 0 1 1))) ; #f



(define (taskIV-сс t s cc)
  (cond 
    ((equal? t 1) (cc s))
    ((equal? t 0) (cc 0))
    (else
     (let ((s_quarter (/ s 4)))
       (taskIV-сс (vector-ref t 0) s_quarter
                  (lambda (x)
                    (taskIV-сс (vector-ref t 1) s_quarter
                               (lambda (y)
                                 (taskIV-сс (vector-ref t 2) s_quarter
                                            (lambda (z)
                                              (taskIV-сс (vector-ref t 3) s_quarter
                                                         (lambda (t)
                                                           (cc (+ t z y x))
                                                         )
                                              )
                                            )
                                 )
                               )
                    )
                  )
       )
     )
    )
  )
)

(println '(My tests for task 4))
(taskIV-сс #(1 0 0 #(1 1 1 0)) 16 (lambda (x) x))
(taskIV-сс #(1 #(0 1 0 1) 0 #(1 1 1 0)) 16 (lambda (x) x)) ; 9



(define taskV
  (lambda args 
    (lambda (input)
      (let loop ((lst (reverse args)) (res input))
        (if (null? lst)
            res
            (loop (cdr lst) ((car lst) res))
        )
      )
    )
  )
)

(println '(My tests for task 5))
((taskV (lambda (x) (* x x)) (lambda (x) (* x 2)) (lambda (x) (+ 1 x)) (lambda (x) (* x 3))) 10) ; ((10*3+1)*2)^2=3844
((taskV (lambda (x) (* x x)) (lambda (x) (+ x 5)) (lambda (x) (* x 2)) (lambda (x) (* x 3))) 1) ; (1*3*2+5)^2=121