#lang racket

(require malt)

(define line (λ (x) (λ (theta) (+ (* (list-ref theta 0) x) (list-ref theta 1)))))

(define l2-loss
  (λ (target)
    (λ (xs ys) (λ (theta) (let ([pred-ys ((target xs) theta)]) (sum (sqr (- ys pred-ys))))))))

(module+ main
  (begin
    (define line-xs (tensor 2 1 4 3))
    (define line-ys (tensor 1.8 1.2 4.2 3.3))
    (define alpha 0.01)
    (define theta (list (- 0 (* alpha -62.63)) 0))
    (displayln ((line line-xs) theta))
    (displayln (((l2-loss line) line-xs line-ys) theta))))
