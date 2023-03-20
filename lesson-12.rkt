#lang racket

(require malt)

(define block (λ (fn shape-lst) (list fn shape-lst)))

(define block-fn (λ (ba) (list-ref ba 0)))

(define block-ls (λ (ba) (list-ref ba 1)))

(define block-compose (λ (f g j) (λ (t) (λ (theta) ((g ((f t) theta)) (refr theta j))))))

(define stack2
  (λ (ba bb)
    (block (block-compose (block-fn ba) (block-fn bb) (tlen (block-ls ba)))
           (append (block-ls ba) (block-ls bb)))))

(define stack-blocks (λ (bls) (stacked-blocks (refr bls 1) (list-ref bls 0))))

(define stacked-blocks
  (λ (rbls ba)
    (cond
      [(null? rbls) ba]
      [else (stacked-blocks (refr rbls 1) (stack2 ba (list-ref rbls 0)))])))

(define dense-block (λ (n m) (block relu (list (list m n) (list m)))))

(module+ main
  (begin
    (displayln 0)))
