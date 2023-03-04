#lang racket

(require malt)

(declare-hyper revs)
(declare-hyper alpha)
(declare-hyper batch-size)

(define l2-loss
  (λ (target)
    (λ (xs ys) (λ (theta) (let ([pred-ys ((target xs) theta)]) (sum (sqr (- ys pred-ys))))))))

(define revise
  (λ (f revs theta)
    (cond
      [(zero? revs) theta]
      [else (revise f (sub1 revs) (f theta))])))

(define gradient-descent
  (λ (obj theta)
    (let ([f (λ (big-theta) (map (λ (p g) (- p (* alpha g))) big-theta (gradient-of obj big-theta)))])
      (revise f revs theta))))

(define samples (λ (n s) (sampled n s (list))))

(define sampled
  (λ (n i a)
    (cond
      [(zero? i) a]
      [else (sampled n (sub1 i) (cons (random n) a))])))

(define sampling-obj
  (λ (expectant xs ys)
    (let ([n (tlen xs)])
      (λ (theta) (let ([b (samples n batch-size)]) ((expectant (trefs xs b) (trefs ys b)) theta))))))

(define line (λ (x) (λ (theta) (+ (* (list-ref theta 0) x) (list-ref theta 1)))))
(define line-xs (tensor 2 1 4 3))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(define plane (λ (t) (λ (theta) (+ (dot-product (list-ref theta 0) t) (list-ref theta 1)))))
(define plane-xs
  (tensor (tensor 1 2.05) (tensor 1 3) (tensor 2 2) (tensor 2 3.91) (tensor 3 6.13) (tensor 4 8.09)))
(define plane-ys (tensor 13.99 15.99 18 22.4 30.2 37.94))

(module+ main
  (begin
    (define result-line
      (with-hypers ((revs 1000) (alpha 0.01) (batch-size 4))
                   (gradient-descent (sampling-obj (l2-loss line) line-xs line-ys) (list 0 0))))
    (displayln result-line)
    (define result-plane
      (with-hypers ((revs 15000) (alpha 0.001) (batch-size 4))
                   (gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                                     (list (tensor 0 0) 0))))
    (displayln result-plane)))
