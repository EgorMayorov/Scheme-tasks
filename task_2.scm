#lang racket

; Итерационно-линейная версия функции с использованием именованного let
(define (odd-fib-list n)
  (if (and (> n 0) (integer? n))
      (if (= n 1)
          '(1)
          (let my-fib ((lst '(1 1)) (out_lst '(1 1)))
            (if (< (length out_lst) n)
                (if (not (even? (+ (car lst) (car (cdr lst)))))
                    (my-fib (append (list(+ (car lst) (car (cdr lst)))) lst) (append out_lst (list(+ (car lst) (car (cdr lst))))))
                    (my-fib (append (list(+ (car lst) (car (cdr lst)))) lst) out_lst)
                )
                out_lst
            )
          )
      )
      (list)
  )
)


; Рекурсивно-линейная версия функции
(define (odd-fib-list-1 n)
  (define (create-list a b lst len)
    (if (< len n)
        (if (odd? (+ a b))
            (append (create-list (+ a b) a lst (+ len 1)) (list (+ a b)))
            (create-list (+ a b) a lst len)
        )
        lst
    )
  )
  (if (and (> n 0) (integer? n))
      (if (not (= n 1))
          (append '(1 1) (reverse (create-list 1 1 '() 2)))
          '(1)
      )
      '()
  )
)

(odd-fib-list 10)
(odd-fib-list-1 10)
