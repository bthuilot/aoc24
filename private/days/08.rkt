#|

Day 8
https://adventofcode.com/2024/day/08


Easy day.

Figured out that the problem was really just asking you
to calculate the slope formed by each pair of the same letters.

The first part was just finding the next point on both sides of the lined
formed between the pair of letters.

The second part ask you to find every point in the line
(within bounds).

|#

#lang racket/base

(require racket/string)
(require racket/list)
(require racket/set)
(require "../solution.rkt")
(require "../utils/point.rkt")
(require "../utils/matrix.rkt")
(require "../utils/lists.rkt")

(provide run)

;; Antennas = [Hashof Character -> [Listof Point]]

;; String -> Antennas Number Number
(define (parse-antennas input)
  (define matrix (map string->list (string-split input "\n")))
  (define (add-point h l p) (hash-set h l (cons p (hash-ref h l list))))
  ;; Antennas [Listof Character] Point -> Antennas
  ;; parses each point in row
  ;; adds to 'as'
  (define (parse-row-point as row p)
    (cond
      [(empty? row) as]
      [else
       (define cur (car row))
       (define remaining (cdr row))
       (define is-antenna? (not (equal? #\. (car row))))
       (define updated-as (if is-antenna? (add-point as cur p) as))
       (parse-row-point updated-as 
                        remaining
                        (translate p 1 0))]))
  ;; parses each row in the matrix 'm'
  (define (parse-row as m p)
    (if (empty? m) as
        (parse-row
         (parse-row-point as (car m) p)
         (cdr m)
         (translate p 0 1))))
  (values
   ;; return the atnenna map
   (parse-row (hash) matrix (point 0 0))
   ;; Max X
   (length (car matrix))
   ;; Max Y
   (length matrix)))

;; Number Number -> [Point Point -> [Listof Point]]
;; Returns a function that for two points, determines
;; all anti-nodes following the 'part 1' descrption
(define ((part1-antinode max-x max-y) p1 p2)
  ;; find slope of two points
  (define s (slope-of p1 p2))
  ;; using slope find the two points
  ;; before and after the pair, if connected
  ;; by a line
  (define after (translate-slope p2 s))
  (define before (translate-slope p1 (negate-slope s)))
  (filter
   ;; filter out any if they are out of bounds
   (λ (p) (point-within? p max-x max-y))
   (list before after)))

;; Number Number -> [Point Point -> [Listof Point]]
;; Returns a function that for two points, determines
;; all anti-nodes following the 'part 2' descrption
(define ((part2-antinode max-x max-y) p1 p2)
  (define s (slope-of p1 p2))
  ;; using slope find the all points (that are in bounds)
  ;; before and after the pair, if connected
  ;; by a line
  (define (points-to-bounds p s acc)
    (define next (translate-slope p s))
    (cond [(not (point-within? next max-x max-y)) acc]
          [else (points-to-bounds next s (cons next acc))]))
  (append (list p1 p2)
          (points-to-bounds p2 s '())
          (points-to-bounds p1 (negate-slope s) '())))


;; [Point Point -> [Listof Point]] Antennas -> [Setof Point]
;; finds all antinodes for a table of Antennas
;; using the function 'an-func' to determine the antinodes
;; generated from two points of the same antenna type
(define (find-antinodes an-func am)
  (define all-points (hash-values am))
  (define all-antinodes (foldl
                         (λ (p acc) (append acc (map-pair an-func p)))
                         '()
                         all-points))
  (apply set all-antinodes))

(define (run input)
  (define-values (antennas max-x max-y) (parse-antennas input))
  (define part1-antinode-func (part1-antinode max-x max-y))
  (define part2-antinode-func (part2-antinode max-x max-y))
  (full-solution
   (set-count (find-antinodes part1-antinode-func antennas))
   (set-count (find-antinodes part2-antinode-func antennas))))


(module+ test
  (require rackunit)

  (define example-input #<<EOF
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
EOF
    )

  (define-values (example-antennas max-x max-y) (parse-antennas example-input))

  (check-equal? example-antennas
                (hash #\0 `(,(point 4 4)
                            ,(point 7 3)
                            ,(point 5 2)
                            ,(point 8 1))
                      #\A `(,(point 9 9)
                            ,(point 8 8)
                            ,(point 6 5))
                      ))
  (check-equal? max-x 12)
  (check-equal? max-y 12)

  
  (define part1-an-f (part1-antinode max-x max-y))
  (define points (find-antinodes part1-an-f example-antennas))
  ;; (print points)
  (check-equal? (set-count points)
                14)

  (define part2-an-f (part2-antinode max-x max-y))

  (define points-2 (find-antinodes part2-an-f example-antennas))
  (check-equal? (set-count points-2)
                34)
  )
