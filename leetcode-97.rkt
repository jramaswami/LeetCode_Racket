;; LeetCode :: July 2022 Challenge :: 97. Interleaving String
;; jramaswami

#lang racket

(define/contract (is-interleave s1 s2 s3)
  (-> string? string? string? boolean?)

  (define s1-length (string-length s1))
  (define s2-length (string-length s2))
  (define s3-length (string-length s3))

  (define (string-ref? str x)
    (if (>= x (string-length str))
        #\?
        (string-ref str x)))

  (define cache (make-hash))
  
  (define (solve i j k)
    (if (>= k s3-length)
        #t
        (let ([key (list i j k)])
          (when (not (hash-has-key? cache key))
            (hash-set! cache key
                       (let ([a (string-ref? s1 i)]
                             [b (string-ref? s2 j)]
                             [c (string-ref? s3 k)])
                         (cond [(and (equal? a c) (equal? b c))
                                (or (solve (+ 1 i) j (+ 1 k))
                                    (solve i (+ 1 j) (+ 1 k)))]
                               [(equal? a c)
                                (solve (+ 1 i) j (+ 1 k))]
                               [(equal? b c)
                                (solve i (+ 1 j) (+ 1 k))]
                               [else #f]))))
          (hash-ref cache key))))        
  
  (if (= (+ s1-length s2-length) s3-length)
      (solve 0 0 0)
      #f))

(module+ test
  (require rackunit)
  (let ([s1 "aabcc"]
        [s2 "dbbca"]
        [s3 "aadbbcbcac"])
    (check-equal? (is-interleave s1 s2 s3) #t))
  (let ([s1 "aabcc"]
        [s2 "dbbca"]
        [s3 "aadbbbaccc"])
    (check-equal? (is-interleave s1 s2 s3) #f))
  (let ([s1 ""]
        [s2 ""]
        [s3 ""])
    (check-equal? (is-interleave s1 s2 s3) #t))
  ;; TLE
  (let ([s1 "bbbbbabbbbabaababaaaabbababbaaabbabbaaabaaaaababbbababbbbbabbbbababbabaabababbbaabababababbbaaababaa"]
        [s2 "babaaaabbababbbabbbbaabaabbaabbbbaabaaabaababaaaabaaabbaaabaaaabaabaabbbbbbbbbbbabaaabbababbabbabaab"]
        [s3 "babbbabbbaaabbababbbbababaabbabaabaaabbbbabbbaaabbbaaaaabbbbaabbaaabababbaaaaaabababbababaababbababbbababbbbaaaabaabbabbaaaaabbabbaaaabbbaabaaabaababaababbaaabbbbbabbbbaabbabaabbbbabaaabbababbabbabbab"])
    (check-equal? (is-interleave s1 s2 s3) #f))
  )