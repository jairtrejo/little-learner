#lang racket

(define remainder
  (lambda (x y)
    (cond
      [(< x y) x]
      [else (remainder (- x y) y)])))

(define add
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (add n (sub1 m)))])))

(module+ main
  (begin
    (displayln (remainder 13 4))
    (displayln (add 7 2))))
