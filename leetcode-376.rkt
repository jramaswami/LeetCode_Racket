;; LeetCode :: July 2022 Challenge :: 376. Wiggle Subsequence
;; jramaswami

#lang racket

(define/contract (wiggle-max-length nums)
  (-> (listof exact-integer?) exact-integer?)

  (define nums0 (list->vector nums))
  (define N (vector-length nums0))

  (define (start-with i)
    (if (> i N)
        0
        (let ([x (vector-ref nums0 i)]
              [j (+ 1 i)])
          (+ 1
             (max
              (choose-next-larger x j)
              (choose-next-smaller x j))))))

  (define (choose-next-larger curr i)
    (if (= i N)
        0
        (let ([x (vector-ref nums0 i)]
              [j (+ 1 i)])
          (if (< curr x)
              (max (+ 1 (choose-next-smaller x j))
                   (choose-next-larger curr j))
              (choose-next-larger curr j)))))

  (define (choose-next-smaller curr i)
    (if (= i N)
        0
        (let ([x (vector-ref nums0 i)]
              [j (+ 1 i)])
          (if (> curr x)
              (max (+ 1 (choose-next-larger x j))
                   (choose-next-smaller curr j))
              (choose-next-smaller curr j)))))

  ;; Memoize functions
  (define (memoize f)
    (let ([cache (make-hash)])
      (lambda (x y)
        (let ([key (list x y)])
          (if (hash-has-key? cache key)
              (hash-ref cache key)
              (let ([result (f x y)])
                (hash-set! cache key result)
                result))))))

  (set! choose-next-larger (memoize choose-next-larger))
  (set! choose-next-smaller (memoize choose-next-smaller))
  
  (define (loop i)
    (if (= i N)
        0
        (max (start-with i) (loop (+ 1 i)))))

  (loop 0))

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