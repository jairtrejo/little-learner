#lang racket

(require malt/interlude-v)

(define of-rank?
  (λ (n t)
    (cond
      [(zero? n) (scalar? t)]
      [(scalar? t) #f]
      [else (of-rank? (sub1 n) (tref t 0))])))

(define ext1
  (λ (f n)
    (λ (t)
      (cond
        [(of-rank? n t) (f t)]
        [else (tmap (ext1 f n) t)]))))

(define rectify-0
  (λ (s)
    (cond
      ((< s 0) 0)
      (else s))))

(define rectify (ext1 rectify-0 0))

(define linear-1-1
  (λ (t)
    (λ (theta)
      (+ (dot-product (list-ref theta 0) t) (list-ref theta 1)))))

(define relu-1-1
  (λ (t)
    (λ (theta)
      (rectify ((linear-1-1 t) theta)))))

(module+ main
  (begin
    (define relu-result ((relu-1-1 (tensor 0.5)) (list (tensor 1) -1)))
    ;(define relu-result (rectify-0  5))
    (displayln relu-result)))
