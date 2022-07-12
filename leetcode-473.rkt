;; LeetCode :: July 2022 Challenge :: 473. Matchsticks to Square
;; jramaswami

#lang racket

(require math/number-theory)

(define/contract (makesquare matchsticks)
  (-> (listof exact-integer?) boolean?)

  (define S (foldr + 0 matchsticks))
  (define T (/ S 4))

  (define cache (make-hash))
  
  (define (solve matchsticks0 A B C D)
    (define key (list matchsticks0 A B C D))
    (when (not (hash-has-key? cache key))
      (hash-set! cache key
                 (cond [(or (> A T) (> B T) (> C T) (> D T)) #f]
                       [(empty? matchsticks0) #t]
                       [else (let ([hd (first matchsticks0)]
                                   [tl (rest matchsticks0)])
                               (or
                                (solve tl (+ A hd) B C D)
                                (solve tl A (+ B hd) C D)
                                (solve tl A B (+ C hd) D)
                                (solve tl A B C (+ D hd))))])))
    (hash-ref cache key))

  (if (divides? 4 S)
      (solve matchsticks 0 0 0 0)
      #f)
  )

(module+ test
  (require rackunit)
  (let ([matchsticks (list 1 1 2 2 2)]
        [expected #t])
    (check-equal? (makesquare matchsticks) expected))
  (let ([matchsticks (list 3 3 3 3 4)]
        [expected #f])
    (check-equal? (makesquare matchsticks) expected))
  ;; TLE
  (let ([matchsticks (list 12 8 12 16 20 24 28 32 36 40 44 48 52 56 60)]
        [expected #f])
    (check-equal? (makesquare matchsticks) expected))
  ;; TLE
  (let ([matchsticks (list 5969561 8742425 2513572 3352059 9084275 2194427
                           1017540 2324577 6810719 8936380 7868365 2755770
                           9954463 9912280 4713511)]
        [expected #f])
    (check-equal? (makesquare matchsticks) expected))
  )