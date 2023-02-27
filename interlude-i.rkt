#lang racket

(require malt)

(define sum-1 (λ (t) (summed-1 t (sub1 (tlen t)) 0)))

(define summed-1
  (λ (t i a)
    (cond
      [(zero? i) (+ a (tref t 0))]
      [else (summed-1 t (sub1 i) (+ a (tref t i)))])))

(module+ main
  (begin
    (displayln (sum-1 (tensor 1 2 3)))))
