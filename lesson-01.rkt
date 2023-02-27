#lang racket

(require malt)

(define line (λ (x) (λ (theta) (+ (* (list-ref theta 0) x) (list-ref theta 1)))))

(module+ main
  (begin
    (displayln ((line (tensor 1 2 3)) (list 1 0)))))
