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
  (λ (a-gradient-descent)
    (with-hypers ((revs 15000) (alpha 0.001) (batch-size 4))
                 (a-gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                                     (list (tensor 0 0) 0)))))

(define lonely-i (λ (p) (list p)))
(define lonely-d (λ (big-p) (list-ref big-p 0)))
(define lonely-u (λ (big-p g) (list (- (list-ref big-p 0) (* alpha g)))))

(define lonely-gradient-descent (gradient-descent lonely-i lonely-d lonely-u))

(define naked-i (λ (p) (let ([big-p p]) big-p)))
(define naked-d (λ (big-p) (let ([p big-p]) p)))
(define naked-u (λ (big-p g) (- big-p (* alpha g))))

(define naked-gradient-descent (gradient-descent naked-i naked-d naked-u))

(module+ main
  (begin
    (define lonely-result (try-plane lonely-gradient-descent))
    (displayln lonely-result)
    (define naked-result (try-plane naked-gradient-descent))
    (displayln naked-result)))
