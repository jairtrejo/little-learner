#lang racket

(require malt)

(declare-hyper revs)
(declare-hyper alpha)
(declare-hyper batch-size)
(declare-hyper mu)
(declare-hyper beta)

(define epsilon 1e-8)

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
  (λ (a-gradient-descent a-revs an-alpha)
    (with-hypers ((revs a-revs) (alpha an-alpha) (batch-size 4))
                 (a-gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                                     (list (tensor 0 0) 0)))))

(define smooth (λ (decay-rate average g) (+ (* decay-rate average) (* (- 1 decay-rate) g))))

(define rms-i (λ (p) (list p (zeroes p))))
(define rms-d (λ (big-p) (list-ref big-p 0)))
(define rms-u
  (λ (big-p g)
    (let ([r (smooth beta (list-ref big-p 1) (sqr g))])
      (let ([alpha-hat (/ alpha (+ epsilon (sqrt r)))])
        (list (- (list-ref big-p 0) (* alpha-hat g)) r)))))

(define rms-gradient-descent (gradient-descent rms-i rms-d rms-u))

(define adam-i (λ (p) (let ([v (zeroes p)]) (let ([r v]) (list p v r)))))
(define adam-d (λ (big-p) (list-ref big-p 0)))
(define adam-u
  (λ (big-p g)
    (let ([r (smooth beta (list-ref big-p 2) (sqr g))])
      (let ([alpha-hat (/ alpha (+ epsilon (sqrt r)))] [v (smooth mu (list-ref big-p 1) g)])
        (list (- (list-ref big-p 0) (* alpha-hat v)) v r)))))

(define adam-gradient-descent (gradient-descent adam-i adam-d adam-u))

(module+ main
  (begin
    (define rms-result (with-hypers ((beta 0.9)) (try-plane rms-gradient-descent 3000 0.01)))
    (displayln rms-result)
    (define adam-result
      (with-hypers ((mu 0.85) (beta 0.9)) (try-plane adam-gradient-descent 1500 0.01)))
    (displayln adam-result)))
