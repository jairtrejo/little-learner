#lang racket

(require malt)

(define line (λ (x) (λ (theta) (+ (* (list-ref theta 0) x) (list-ref theta 1)))))

(define l2-loss
  (λ (target)
    (λ (xs ys) (λ (theta) (let ([pred-ys ((target xs) theta)]) (sum (sqr (- ys pred-ys))))))))

(define revise
  (λ (f revs theta)
    (cond
      [(zero? revs) theta]
      [else (revise f (sub1 revs) (f theta))])))

(define revs 1000)
(define alpha 0.01)

(define gradient-descent
  (lambda (obj theta)
    (let ([f (λ (big-theta) (map (λ (p g) (- p (* alpha g))) big-theta (gradient-of obj big-theta)))])
      (revise f revs theta))))

(module+ main
  (begin
    (define line-xs (tensor 2 1 4 3))
    (define line-ys (tensor 1.8 1.2 4.2 3.3))
    (define result (gradient-descent ((l2-loss line) line-xs line-ys) (list 0 0)))
    (displayln result)))
