;; LeetCode :: July 2022 Challenge :: 1465. Maximum Area of a Piece of Cake After Horizontal and Vertical Cuts
;; jramaswami

#lang racket

(require racket/contract)
(require math/number-theory)

(define/contract (max-area height width horizontal-cuts vertical-cuts)
  (-> exact-integer? exact-integer? (listof exact-integer?) (listof exact-integer?) exact-integer?)

  (define (max-cuts0 cuts dim)
    (if (empty? (rest cuts))
        (- dim (first cuts)) 
        (max (- (second cuts) (first cuts))
             (max-cuts0 (rest cuts) dim))))
  
  (define (max-cuts cuts dim)
    (max (first cuts)
         (max-cuts0 cuts dim)))

  (with-modulus (+ (expt 10 9) 7)
    (mod* (max-cuts (sort horizontal-cuts <) height)
          (max-cuts (sort vertical-cuts <) width))))

(module+ test
  (require rackunit)
  (let ([height 5]
        [width 4]
        [horizontal-cuts (list 1 2 4)]
        [vertical-cuts (list 1 3)]
        [expected 4])
    (check-equal? (max-area height width horizontal-cuts vertical-cuts) expected))
  (let ([height 5]
        [width 4]
        [horizontal-cuts (list 3 1)]
        [vertical-cuts (list 1)]
        [expected 6])
    (check-equal? (max-area height width horizontal-cuts vertical-cuts) expected))
  (let ([height 5]
        [width 4]
        [horizontal-cuts (list 3)]
        [vertical-cuts (list 3)]
        [expected 9])
    (check-equal? (max-area height width horizontal-cuts vertical-cuts) expected))
  )