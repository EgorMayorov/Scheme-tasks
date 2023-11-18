#lang racket
(define (stream-scale s f)
  (stream-map (lambda (x) (* x f)) s)
)

(define (step x)
  (stream-cons 1 (stream-scale (step x) x))
)

(define (combine str1 str2)
  (stream-cons (stream-first str1)
               (combine str2 (stream-rest str1))
  )
)

(define (step-streams s1 s2)
  (if (< (stream-first s1) (stream-first s2))
      (stream-cons (stream-first s1) (step-streams (stream-rest s1) s2))
      (stream-cons (stream-first s2) (step-streams s1 (stream-rest s2)))
  )
)

(define step-stream
  (step-streams (stream-rest (step 2)) (step 3))
)

(define (print-first-n st n)
  (let loop ((i 0))
    (if (< i n)
        (begin
          (println (stream-ref st i))
          (loop (+ i 1))
        )
        (void)
    )
  )
)


(print-first-n step-stream 15)