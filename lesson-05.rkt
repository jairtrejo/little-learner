#lang racket

(require malt)

(define l2-loss
  (λ (target)
    (λ (xs ys) (λ (theta) (let ([pred-ys ((target xs) theta)]) (sum (sqr (- ys pred-ys))))))))

(define revise
  (λ (f revs theta)
    (cond
      [(zero? revs) theta]
      [else (revise f (sub1 revs) (f theta))])))

(declare-hyper revs)
(declare-hyper alpha)

(define gradient-descent
  (lambda (obj theta)
    (let ([f (λ (big-theta) (map (λ (p g) (- p (* alpha g))) big-theta (gradient-of obj big-theta)))])
      (revise f revs theta))))

(define line (λ (x) (λ (theta) (+ (* (list-ref theta 0) x) (list-ref theta 1)))))

(define quad
  (λ (t)
    (λ (theta) (+ (* (list-ref theta 0) (sqr t)) (+ (* (list-ref theta 1) t) (list-ref theta 2))))))

(define plane (λ (t) (λ (theta) (+ (dot-product (list-ref theta 0) t) (list-ref theta 1)))))

(module+ main
  (begin
    ; line
    (define line-xs (tensor 2 1 4 3))
    (define line-ys (tensor 1.8 1.2 4.2 3.3))
    (define result
      (with-hypers ((revs 1000) (alpha 0.01))
                   (gradient-descent ((l2-loss line) line-xs line-ys) (list 0 0))))
    (displayln result)
    ;quad
    (define quad-xs (tensor -1 0 1 2 3))
    (define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))
    (define result-quad
      (with-hypers ((revs 1000) (alpha 0.001))
                   (gradient-descent ((l2-loss quad) quad-xs quad-ys) (list 0 0 0))))
    (displayln result-quad)
    ; plane
    (define plane-xs
      (tensor (tensor 1 2.05)
              (tensor 1 3)
              (tensor 2 2)
              (tensor 2 3.91)
              (tensor 3 6.13)
              (tensor 4 8.09)))
    (define plane-ys (tensor 13.99 15.99 18 22.4 30.2 37.94))
    (define result-plane
      (with-hypers ((revs 1000) (alpha 0.001))
                   (gradient-descent ((l2-loss plane) plane-xs plane-ys) (list (tensor 0 0) 0))))
    (displayln result-plane)))
