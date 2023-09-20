#lang racket

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

(odd-fib-list 7)
