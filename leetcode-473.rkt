;; LeetCode :: July 2022 Challenge :: 473. Matchsticks to Square
;; jramaswami

#lang racket

(require math/number-theory)

(define/contract (makesquare matchsticks)
  (-> (listof exact-integer?) boolean?)

  (define S (foldr + 0 matchsticks))
  (define T (/ S 4))
  
  (define (solve matchsticks0 A B C D)
    (cond [(or (> A T) (> B T) (> C T) (> D T)) #f]
          [(empty? matchsticks0) #t]
          [else (let ([hd (first matchsticks0)]
                      [tl (rest matchsticks0)])
                  (or
                   (solve tl (+ A hd) B C D)
                   (solve tl A (+ B hd) C D)
                   (solve tl A B (+ C hd) D)
                   (solve tl A B C (+ D hd))))]))

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
    (check-equal? (makesquare matchsticks) expected)))