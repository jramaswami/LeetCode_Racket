;; LeetCode :: July 2022 Challenge :: 128. Longest Consecutive Sequence
;; jramaswami

#lang racket

(define/contract (longest-consecutive nums)
  (-> (listof exact-integer?) exact-integer?)
  ;; Turn nums into a set with O(1) access(?)
  (define nums-set (list->set nums))

  ;; Return #t if n starts sequence. It starts a sequence if
  ;; n - 1 is not in the set of numbers.
  (define (starts-sequence n)
    (not (set-member? nums-set (- n 1))))

  ;; Compute the length of sequence starting with n.
  (define (compute-sequence-length n)
    (if (set-member? nums-set n)
        (+ 1 (compute-sequence-length (+ 1 n)))
        0))

  (define (sequence-length n)
    (if (starts-sequence n)
        (compute-sequence-length n)
        0))

  (apply max (set-map nums-set sequence-length)))
    

(module+ test
  (require rackunit)
  (let ([nums (list 100 4 200 1 3 2)]
        [expected 4])
    (check-equal? (longest-consecutive nums) expected))
  (let ([nums (list 0 3 7 2 5 8 4 6 0 1)]
        [expected 9])
    (check-equal? (longest-consecutive nums) expected))
  ;; RTE
  (let ([nums '()]
        [expected 0])
    (check-equal? (longest-consecutive nums) expected)))