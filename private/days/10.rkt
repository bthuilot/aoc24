#|

Day 10
https://adventofcode.com/2024/day/10

I really thought a graph problem was gonna be
hard but I got this one a lot quicker than I realized.

First did each part seperately but eventually
settled on one function that computes the 'Point's
for '9' tiles that can be reached from a given point.
points are repeated if they occur from a different path.

I then fold over the whole map with that function
and each result for a 0 tile I add to a list.

If for each 0 value you compute the amount of unique
points in each list, that is the 'score'.

If for each 0 value you get the amount of points in each
list, thats the 'rating'

|#

#lang racket/base

(require racket/function)
(require racket/string)
(require racket/set)
(require racket/list)
(require "../utils/matrix.rkt")
(require "../utils/strings.rkt")
(require "../utils/point.rkt")
(require "../solution.rkt")

(provide run)

;; TopographicMap = [Matrixof Integer]

;; String -> TopographicMap
;; Parses the topographic map from the
;; given string
(define (parse-topographic-map input)
  (define lines (string-split input "\n"))
  (map (λ (l) (map char->number (string->list l))) lines))

;; TrailTails = [Listof Point]
;; represents the points for the ends
;; of the trails reached, the same point
;; visited through a different path will
;; contain duplicated values

;; Cache = [Hashof Point -> TrailTails]
;; cache records the computed results of TrailTails

;; TopographicMap Cache Number Point -> (cons Cache TrailTails)
;; returns the TrailTails for a given point with the given height
;; cache is used to store the TrailTails for other points as to not
;; re-compute previously explored
(define (trailtails t-map cache height point)
  ; fold-next will find the TrailTrails for all the next points
  ; and combine their output into a single TrailTrails
  (define (fold-next cur-height next-point acc)
    (define next-height (matrix-point t-map next-point))
    (define c (car acc)) ; cache
    (define tails (cdr acc))
    (cond
      [(not (= next-height (add1 cur-height))) (cons c tails)]
      [else
       (define-values (tm ta) (trailtails t-map c next-height next-point))
       (cons tm (append ta tails))]))
  
  (cond
    ; if were at the top, return the current point
    [(= height 9) (values cache (list point))]
    ; return previous result if possible
    [(hash-has-key? cache point) (values cache (hash-ref cache point))]
    [else
     ; get all the endpoints of the next points,
     ; and combine them
     (define c-points (cardinal-points point))
     (define next-points (filter ((curry in-bounds?) t-map) c-points))
     (define acc (foldl ((curry fold-next) height)
                        (cons cache '())
                        next-points))
     (define tails (cdr acc))
     (define final-cache (hash-set (car acc) point tails))
     (values final-cache tails)]))


;; TopographicMap -> [Listof TrailTails]
;; computes the TrailTrails for every point
;; and then returns a list of the TrailsTails
;; for all points that have height 0
(define (calculate-trailhead-tails t-map)
  (define (fold point height acc)
    (define cache (car acc))
    (define all-tails (cdr acc))
    (define-values (update-cache tails) (trailtails t-map cache height point))
    (cons update-cache
          (if (= height 0) (cons tails all-tails) all-tails)))
  (cdr (matrix-fold-point fold (cons (hash) '()) t-map)))

;; TrailTrails Number -> Number
(define (sum-score tails total)
  (+ (set-count (list->set tails)) total))

;; TrailTrails Number -> Number
(define (sum-ratings tails total)
  (+ (length tails) total))

;; TrailsTails -> Number
(define (part1 tails)
  (foldl sum-score 0 tails))

;; TrailsTails -> Number
(define (part2 tails)
  (foldl sum-ratings 0 tails))

(define (run input)
  (define t-map (parse-topographic-map input))
  (define tails (calculate-trailhead-tails t-map))
  (full-solution (part1 tails) (part2 tails)))

(module+ test
  (require rackunit)

  (define example-input #<<EOF
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
EOF
    )


  (define t-map (parse-topographic-map example-input))
  (check-equal? t-map
                '((8 9 0 1 0 1 2 3)
                  (7 8 1 2 1 8 7 4)
                  (8 7 4 3 0 9 6 5)
                  (9 6 5 4 9 8 7 4)
                  (4 5 6 7 8 9 0 3)
                  (3 2 0 1 9 0 1 2)
                  (0 1 3 2 9 8 0 1)
                  (1 0 4 5 6 7 3 2)))

  (define tails (calculate-trailhead-tails t-map))

  (check-equal? (part1 tails)
                36)
  (check-equal? (part2 tails)
                81)

  )
