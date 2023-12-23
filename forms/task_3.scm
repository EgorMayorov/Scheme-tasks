#lang racket

; Вспомогательные функции:
; Линейно-итерационная функция построения списка из n чисел Фибоначчи
(define (my-fib n)
  (let my-inner-fib ((lst '(1 1)))
    (if (< (length lst) n)
        (my-inner-fib (append (list(+ (car lst) (car (cdr lst)))) lst))        
        (reverse lst)
    )
  )
)

; Функция, возвращающая n член ряда Фибоначчи
(define (fib n)
  (let loop ((k n) (n1 0) (acc 1))
    (if (= k 0)
        acc
        (loop (- k 1) acc (+ acc n1))
    )
  )
)



; 1.a)
; Линейо-итерационный процесс построения списка квадратов первых n чисел Фибоначчи
(define (list-fib-squares-a n)
  (map (lambda (x)
         (let ((temp x)) (* temp temp)))
       ;(my-fib n)    ; Можно использовать вместо build-list
       (build-list n fib)
  )
)

(list-fib-squares-a 7)



; 1.b)
; Линейно-итерационный процесс, использующий только свертку в качестве цикла
(define (list-fib-squares-b n)
  (define (cr-list a lst)
    (append lst (list(* a a)))
  )
  (foldl cr-list '() (build-list n fib))
)

(list-fib-squares-b 7)



; 2)
(define (process lst)
  (let loop ((val (foldl * 1 (car lst))) (inner-lst (cdr lst)) (result '()))
    (if (null? inner-lst) (reverse result)
        (loop val (cdr inner-lst)
                (if (> (foldl + 0 (car inner-lst)) val)
                    (cons (car inner-lst) result)
                    result
                )
        )
    )
  )
)

(process '((5) (1 2) () (3 4) (2 3) (2 3 4)))