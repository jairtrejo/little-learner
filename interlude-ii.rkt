#lang racket

(require malt)

(declare-hyper smaller)

(define nonsense? (λ (x) (= (sub1 x) smaller)))

(module+ main
  (begin
    (displayln (with-hyper ((smaller 5)) (nonsense? 6)))))
