#lang racket

(require math/number-theory)

(define (get-sum-of-x-dividers x)
  (let loop ((k 1) (result '()))
    (cond
      ((> k x) (foldl + 0 result))
      ((divides? k x) (loop (+ k 1) (cons k result)))
      (else (loop (+ k 1) result))
    )
  )
)

(define (is-perfect? x) (= (get-sum-of-x-dividers x) (* 2 x)))

(define (perfect n)
  (let loop ((perfects 0) (k 1))
    (if (is-perfect? k)
        (if (equal? (+ perfects 1) n)
            k
            (loop (+ perfects 1) (+ k 1))
        )
        (loop perfects (+ k 1))
    )
  )
)

; tests
(perfect 1) ; -> 6
(perfect 2) ; -> 28
(perfect 3) ; -> 496
(perfect 4) ; -> 8128
(perfect 4) ; -> 8128
(perfect 4) ; -> 8128



(define hash-table (make-hash '()))

(define (memo-perfect n) 
  (define (get-max-useful-index n)
    (let ((max-of-keys (foldl max 0 (hash-keys hash-table))))
      (min n max-of-keys)
    )
  )
  (let* ((idx (get-max-useful-index n))
         (k (hash-ref hash-table idx 1)))
    (let loop ((perfects (max 0 (- idx 1))) (k k))
      (if (is-perfect? k)
          (begin
            (hash-set! hash-table (+ perfects 1) k) 
            (if (equal? (+ perfects 1) n)
                k
                (loop (+ perfects 1) (+ k 1))
            )
          )
          (loop perfects (+ k 1))
      )
    )
  )
)

(memo-perfect 1) ; -> 6
(memo-perfect 2) ; -> 28
(memo-perfect 3) ; -> 496
(memo-perfect 4) ; -> 8128
(memo-perfect 4) ; -> 8128
(memo-perfect 4) ; -> 8128




; Даёт ли мемоизированная версия выигрыш по сравнению с обычной? Если даёт, то какой и при каких условиях?

; Мемоизованная версия дает выигрыш при сравнении с обычной, в случае, если это не первый вызов функции.
; Она дает возможность не вычислять заново 1...n-1 элементы последовательности при последовательных вызовах функции
; для разных n, а искать сразу следующие члены последовательности. При первом вызове данная реализация не дает выигрыша,
; так как хэш-таблица еще пуста. Но, например, при нескольких одинаковых вызовах дает возможность найти нужное значение
; в таблице, а не вычислять его заново.