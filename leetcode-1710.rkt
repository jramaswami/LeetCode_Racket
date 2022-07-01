;; LeetCode :: June 2022 Challenge :: 1710. Maximum Units on a Truck
;; jramaswami

#lang racket

(require racket/contract)

(define/contract (maximum-units boxTypes truckSize)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer?)

  ;; Helper function to solve recursively
  (define (fill-truck currList currCapacity)
    (if (empty? currList)
        0
        (let* (
               [top (first currList)]
               [numberOfBoxes (first top)]
               [unitsPerBox (second top)])
          (if (> numberOfBoxes currCapacity)
              (* unitsPerBox currCapacity)
              (+ (* unitsPerBox numberOfBoxes)
                 (fill-truck (rest currList) (- currCapacity numberOfBoxes)))))))
  
  ;; Sort the boxTypes by units per box   then greedily fill the truck.
  (fill-truck (sort boxTypes (lambda (x y) (> (second x) (second y)))) truckSize))

(module+ test
  (require rackunit)
  (let ([boxTypes (list (list 1 3) (list 2 2) (list 3 1))]
        [truckSize 4]
        [expected 8])
    (check-equal? (maximum-units boxTypes truckSize) expected))
  (let ([boxTypes (list (list 5 10) (list 2 5) (list 4 7) (list 3 9))]
        [truckSize 10]
        [expected 91])
    (check-equal? (maximum-units boxTypes truckSize) expected))
  ;; RTE: did not take into account a truck capacity more than total boxes.
  (let ([boxTypes (list (list 1  3)  (list 5  5)  (list 2  5)  (list 4  2)  (list 4  1)  (list 3  1)  (list 2  2)  (list 1  3)  (list 2  5)  (list 3  2))]
        [truckSize       35]
        [expected 76])
    (check-equal? (maximum-units boxTypes truckSize) expected))
  )