#lang racket

(require malt)

(define dot-product-2-1 (λ (w t) (sum (*-2-1 w t))))

(define linear (λ (t) (λ (theta) (+ (dot-product-2-1 (list-ref theta 0) t) (list-ref theta 1)))))

(define relu (λ (t) (λ (theta) (rectify ((linear t) theta)))))

(define k-relu
  (λ (k)
    (λ (t)
      (λ (theta)
        (cond
          [(zero? k) t]
          [else (((k-relu (sub1 k)) (relu t theta)) (refr theta 2))])))))
