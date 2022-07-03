;; LeetCode :: July 2022 Challenge :: 376. Wiggle Subsequence
;; jramaswami

#lang racket


;; Start with the first number in nums.
(define (start-with-first nums)
  (if (empty? nums)
      0
      (+ 1
         (max
          (choose-next-larger (first nums) (rest nums))
          (choose-next-smaller (first nums) (rest nums))))))

;; Choose a number larger than curr.
(define (choose-next-larger curr nums)
  (cond [(empty? nums) 0]
        [(< curr (first nums))
         (max (+ 1 (choose-next-smaller (first nums) (rest nums)))
              (choose-next-larger curr (rest nums)))]
        [else (choose-next-larger curr (rest nums))]))

;; Choose a number smaller than curr.
(define (choose-next-smaller curr nums)
  (cond [(empty? nums) 0]
        [(> curr (first nums))
         (max (+ 1 (choose-next-larger (first nums) (rest nums)))
              (choose-next-smaller curr (rest nums)))]
        [else (choose-next-smaller curr (rest nums))]))

(define/contract (wiggle-max-length nums)
  (-> (listof exact-integer?) exact-integer?)
  (if (empty? nums)
      0
      (max (start-with-first nums)
           (wiggle-max-length (rest nums)))))

(module+ test
  (require rackunit)
  
  ;; Example 1
  (let ([nums (list 1 7 4 9 2 5)]
        [expected 6])
    (check-equal? (wiggle-max-length nums) expected))

  ;; Example 2
  (let ([nums (list 1 17 5 10 13 15 10 5 16 8)]
        [expected 7])
    (check-equal? (wiggle-max-length nums) expected))
  )