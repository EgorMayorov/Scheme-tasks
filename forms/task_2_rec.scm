#lang racket

(define (odd-fib-list n)
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