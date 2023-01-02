;; LeetCode :: July 2022 Challenge :: 473. Matchsticks to Square
;; jramaswami

#lang racket

(require math/number-theory)

;; Given a bitmaks, return #t if the selected matchsticks
;; sum to side-length.
(define (can-form-side? bitmask side-length matchsticks)
  (= side-length
     (for/sum ([b (range 15)]
               [m matchsticks])
       (if (bitwise-bit-set? bitmask b)
           m
           0))))

(define (possible-bitmasks side-length matchsticks)
  (filter
   (lambda (bitmask) (can-form-side? bitmask side-length matchsticks))
   (range (expt 2 (length matchsticks)))))

;; Find four sides that can go together with no overlap of matches.
(define (find-four possible-sides sides-found curr-selection)
  (cond [(= sides-found 4) #t]
        [(empty? possible-sides) #f]
        [(= (bitwise-xor (first possible-sides) curr-selection)
            (+ (first possible-sides) curr-selection))
         (or
          (find-four
           (rest possible-sides) (+ 1 sides-found)
           (+ (first possible-sides) curr-selection))
          (find-four
           (rest possible-sides) sides-found
           curr-selection))]
        [else
         (find-four
          (rest possible-sides) sides-found
          curr-selection)]))
        
(define/contract (makesquare matchsticks)
  (-> (listof exact-integer?) boolean?)
  (define N (length matchsticks))
  (define total-length (foldr + 0 matchsticks))
  (define side-length (/ total-length 4))
  (cond [(not (divides? 4 total-length)) #f]
        [(for/and ([m matchsticks])
           (> m side-length))
         #f]
        [else
         (find-four (possible-bitmasks (/ total-length 4) matchsticks) 0 0)]))

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
  ;; WA
  (let ([matchsticks (list 5 5 5 5 4 4 4 4 3 3 3 3)]
        [expected #t])
    (check-equal? (makesquare matchsticks) expected))
  ;; TLE
  (let ([matchsticks (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6)]
        [expected #f])
    (check-equal? (makesquare matchsticks) expected))
  (let ([matchsticks (list 1 1 1 1 1 1 1 1 1 1 1 1)]
        [expected #t])
    (check-equal? (makesquare matchsticks) expected))
  )