#lang racket

(define-syntax swap
  (syntax-rules ()
    ((swap a b)
     (let ((c b))
       (set! b a)
       (set! a c)
     )
    )
  )
)

(define-syntax right-rot! 
  (syntax-rules ()
    ((right-rot!) (void))
    ((right-rot! el1) el1)
    ((right-rot! el1 el2) (swap el1 el2))
    ((right-rot! el1 el2 ... el-n-1 el-n)
     (begin
       (swap el-n-1 el-n)
       (right-rot! el1 el2 ... el-n-1)
     )
    )
  )
)

(let ((x 1) (y 2) (z 3))
  (begin
    (right-rot! x y z)
    (- x y z)
  )
) ; -> 0

; реализация с помощью функции невозможна, так как аргументы функции вычисляются при вызове функции
; и внутри функции будут изменяться локальные переменные.
; Поэтому возможна реализация только с помощью макроса.
