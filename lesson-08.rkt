#lang racket

(require malt)

(declare-hyper revs)
(declare-hyper alpha)
(declare-hyper batch-size)
(declare-hyper mu)

(define l2-loss
  (λ (target)
    (λ (xs ys) (λ (theta) (let ([pred-ys ((target xs) theta)]) (sum (sqr (- ys pred-ys))))))))

(define revise
  (λ (f revs theta)
    (cond
      [(zero? revs) theta]
      [else (revise f (sub1 revs) (f theta))])))

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

(define gradient-descent
  (λ (inflate deflate update)
    (λ (obj theta)
      (let ([f (λ (big-theta) (map update big-theta (gradient-of obj (map deflate big-theta))))])
        (map deflate (revise f revs (map inflate theta)))))))

(define plane (λ (t) (λ (theta) (+ (dot-product (list-ref theta 0) t) (list-ref theta 1)))))
(define plane-xs
  (tensor (tensor 1 2.05) (tensor 1 3) (tensor 2 2) (tensor 2 3.91) (tensor 3 6.13) (tensor 4 8.09)))
(define plane-ys (tensor 13.99 15.99 18 22.4 30.2 37.94))

(define try-plane
  (λ (a-gradient-descent a-revs)
    (with-hypers ((revs a-revs) (alpha 0.001) (batch-size 4))
                 (a-gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                                     (list (tensor 0 0) 0)))))

(define velocity-i
  (λ (p) (list p (zeroes p))))

(define velocity-d
  (λ (big-p) (list-ref big-p 0)))

(define velocity-u
  (λ (big-p g) (let ((v (- (* mu (list-ref big-p 1)) (* alpha g)))) (list (+ (list-ref big-p 0) v) v))))

(define velocity-gradient-descent
  (gradient-descent velocity-i velocity-d velocity-u))

(module+ main
  (begin
    (define result (with-hypers ((mu 0.9)) (try-plane velocity-gradient-descent 5000)))
    (displayln result)))
