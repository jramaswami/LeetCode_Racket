;; LeetCode :: July 2022 Challenge :: 376. Wiggle Subsequence
;; jramaswami

#lang racket

(define/contract (sign? n)
  (-> exact-integer? exact-integer?)
  (cond [(< n 0) -1]
        [(> n 0) 1]
        [else 0]))

(define/contract (compute-differences nums0)
  (-> (listof exact-integer?) (listof exact-integer?))
  (let ([a (first nums0)]
        [tl (rest nums0)])
    (if (empty? tl)
        '()
        (let ([b (first tl)])
          (cons (- a b) (compute-differences tl))))))

(define/contract (wiggle-pair? a b)
  (-> exact-integer? exact-integer? boolean?)
  (let ([prev-sign (sign? b)]
        [curr-sign (sign? a)])
    (cond [(and (= prev-sign -1) (= curr-sign 1)) #t]
          [(and (= prev-sign 1) (= curr-sign -1)) #t]
          [else #f])))

(define/contract (longest-wiggle differences0 curr-wiggle curr-length max-length)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (if (empty? differences0)
      (max max-length curr-length)
      (if (wiggle-pair? (first differences0) (first curr-wiggle))
          (longest-wiggle (rest differences0) (cons (first differences0) curr-wiggle) (+ 1 curr-length) max-length)
          (longest-wiggle (rest differences0) (cons (first differences0) '()) 1 (max curr-length max-length)))))

(define/contract (wiggle-max-length nums)
  (-> (listof exact-integer?) exact-integer?)
  (define differences (compute-differences nums))
  (fprintf (current-output-port) "~v~n" differences)
  (+ 1 (longest-wiggle (rest differences) (cons (first differences) '()) 1 1)))

(module+ test
  (require rackunit)
  (check-equal? (sign? -6) -1)
  (check-equal? (sign? 6) 1)

  (check-equal? (wiggle-pair? -1 1) #t)
  (check-equal? (wiggle-pair? 1 -1) #t)
  (check-equal? (wiggle-pair? 1 1) #f)
  (check-equal? (wiggle-pair? -1 -1) #f)
  
  ;; Example 1
  (let ([nums (list 1 7 4 9 2 5)]
        [expected 6])
    (check-equal? (wiggle-max-length nums) expected))

  ;; Example 2
  (let ([nums (list 1 17 5 10 13 15 10 5 16 8)]
        [expected 7])
    (check-equal? (wiggle-max-length nums) expected))
  )