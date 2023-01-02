;; LeetCode
;; 520. Detect Capital
;; January 2022 Challenge
;; jramaswami

#lang racket

(define (count-capitals word-chars)
  (cond [(empty? word-chars) 0]
        [(char-lower-case? (first word-chars)) (count-capitals (rest word-chars))]
        [else (+ 1 (count-capitals (rest word-chars)))]))

         
(define/contract (detect-capital-use word)
  (-> string? boolean?)
  (let* ([word-chars (string->list word)]
         [word-length (string-length word)]
         [capital-count (count-capitals word-chars)])
    (cond [(= capital-count word-length) #t]
          [(= capital-count 1) (char-upper-case? (first word-chars))]
          [(= capital-count 0) #t]
          [else #f])))

(module+ test
  (require rackunit)
  ;; Test (count-capitals)
  (check-equal? 3 (count-capitals (string->list "USA")))
  (check-equal? 2 (count-capitals (string->list "FlaG")))
  (check-equal? 0 (count-capitals (string->list "hello")))
  ;; Test 1
  (let* ([word "USA"]
         [expected #t]
         [result (detect-capital-use word)])
    (check-equal? result expected))
  ;; Test 2
  (let* ([word "FlaG"]
         [expected #f]
         [result (detect-capital-use word)])
    (check-equal? result expected))
  ;; Test 3
  (let* ([word "Flag"]
         [expected #t]
         [result (detect-capital-use word)])
    (check-equal? result expected))
  ;; Test 4
  (let* ([word "hello"]
         [expected #t]
         [result (detect-capital-use word)])
    (check-equal? result expected))
  ;; Test 5
  (let* ([word "heLlo"]
         [expected #f]
         [result (detect-capital-use word)])
    (check-equal? result expected))
  )