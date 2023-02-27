#lang racket

(require malt)

(define rank-wrapped
  (lambda (t)
    (cond
      [(scalar? t) 0]
      [else (add1 (rank (tref t 0)))])))

(define rank (lambda (t) (ranked t 0)))

(define ranked
  (lambda (t a)
    (cond
      [(scalar? t) a]
      [else (ranked (tref t 0) (add1 a))])))

(module+ main
  (begin
    (displayln (rank (tensor 6.2 1.0 3.1 2.9)))
    (displayln (rank (tensor (tensor 6.2 1.0) (tensor 3.1 2.9))))))
