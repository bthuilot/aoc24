#|

Day 12
https://adventofcode.com/2024/day/12

So, the first part was easy (relatively)
for each point see how many adjancent points are also
the same plot (area) and how many points are not (permimeter).

Part 2 then stumped me... for like 5 days.
I eventually settled on recording the direction
it took to get to each border point and when calculating
sides, for each border reached from the same direction:
1. group them by the perpendicular axis (to make sure they would
be next to each other)
2. sort them based on the parellel axis (so the coordinates
should be increasing)
3. group them by adjancent (coordinates only change by 1),
and count the amount of groups

Is this the "correct" solution, probably not
but again, this is 5 days late so just rolling with it

|#

#lang racket/base


(require racket/string)
(require racket/function)
(require racket/set)
(require racket/list)
(require "../solution.rkt")
(require "../utils/point.rkt")
(require "../utils/matrix.rkt")

(provide run)

;; Farm = [Matrixof Character]

;; String -> Farm
(define (parse-farm input)
  (map string->list (string-split input)))

;; (measured [Setof Point] [Listof [Pairof Point Direction]])
;; Represents the fenced points of a plot of land
;; and their borders paired with the direction from the
;; plot of land it came from
(struct measured (fenced borders) #:transparent)

;; Measured -> Number
;; Calculates the cost to fence
;; a plot of land based on part 1
(define (cost-f-part1 m)
  (* (set-count (measured-fenced m))
     (length (measured-borders m))))

;; Measured -> Number
;; Calculates the cost to fence
;; a plot of land based on part 2
(define (cost-f-part2 m)
  (define (check axis1 axis2 p)
    (define grouped (map
                     (compose (λ (l) (sort l <)) ((curry map) axis1))
                     (group-by axis2 p)))
    (define (count-adj x xs)
      (cond
        [(empty? xs) 1]
        [(not (= (abs (- x (car xs))) 1)) (+ 1 (count-adj (car xs) (cdr xs)))]
        [else (count-adj (car xs) (cdr xs))]
        )
      )
    (apply + (map (λ (l) (count-adj (car l) (cdr l))) grouped))
    )
  
  (define horizontal-check ((curry check) point-x point-y))
  
  (define vertical-check ((curry check) point-y point-x))
  
  (define (find-sides borders)
    (define ((filter-dir dir) b) (equal? dir (cdr b)))
    (define east (filter (filter-dir 'east) borders))
    (define west (filter (filter-dir 'west) borders))
    (define north (filter (filter-dir 'north) borders))
    (define south (filter (filter-dir 'south) borders))
    
    (+ (horizontal-check (map car north))
       (horizontal-check (map car south))
       (vertical-check (map car east))
       (vertical-check (map car west))))
  
  (* (set-count (measured-fenced m))
     (find-sides (measured-borders m))))

;; Farm Character Point -> Boolean
;; returns true of the given point
;; is the same plot type as the given plot
;; and within bounds of the farm
(define ((same-plot? farm plot) point)
  (and (in-bounds? farm point)
       (equal? plot (matrix-point farm point))))


;; Farm [Measure -> Number] -> Number
;; Calculates the cost to fence the plots
;; on the farm. Uses [cost-f] to determine
;; the cost of a particular plot of land
;; by its measured
(define (cost farm cost-f)
  ; returns the Measured for a plot
  (define ((measure-plot plot) point m)
    (define-values (same-plot-dir borders-dir)
      (partition
       (compose (same-plot? farm plot) car)
       (cardinal-points-dir point)))
    (define b (append (measured-borders m) borders-dir))
    (define next-plots
      ; filter out visited
      (filter (compose not ((curry set-member?) (measured-fenced m)))
              ; remove direction
              (map car same-plot-dir)))
    (define f (set-union (measured-fenced m) (list->set next-plots)))
    (foldl (measure-plot plot) (measured f b) next-plots))

  ;; folds over all points in the farm and
  ;; calculates the cost of fencing each plot
  (define (foldf point plot acc)
    (define fenced (car acc))
    (define cost (cdr acc))
    (cond
      [(set-member? fenced point) acc]
      [else
       (define m ((measure-plot plot) point (measured (set point) '())))
       (cons
        (set-union fenced (measured-fenced m))
        (+ cost (cost-f m)))]))

  (cdr (matrix-fold-point foldf (cons (set) 0) farm)))


(define (run input)
  (define farm (parse-farm input))
  (full-solution
   (cost farm cost-f-part1)
   (cost farm cost-f-part2)))


(module+ test
  (require rackunit)

  (define example-1 #<<EOF
AAAA
BBCD
BBCC
EEEC
EOF
    )

  (define example-2 #<<EOF
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
EOF
    )

  (define example-3 #<<EOF
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
EOF
    )

  (define farm-1 (parse-farm example-1))
  (define farm-2 (parse-farm example-2))
  (define farm-3 (parse-farm example-3))

  (check-equal? farm-1
                '((#\A #\A #\A #\A)
                  (#\B #\B #\C #\D)
                  (#\B #\B #\C #\C)
                  (#\E #\E #\E #\C)))

  (check-equal? farm-2
                '((#\O #\O #\O #\O #\O)
                  (#\O #\X #\O #\X #\O)
                  (#\O #\O #\O #\O #\O)
                  (#\O #\X #\O #\X #\O)
                  (#\O #\O #\O #\O #\O)))

  (check-equal? (cost farm-1 cost-f-part1)
                140)

  (check-equal? (cost farm-2 cost-f-part1)
                772)
  (check-equal? (cost farm-3 cost-f-part1)
                1930)
  
  (check-equal? (cost farm-1 cost-f-part2)
                80)

  (check-equal? (cost farm-2 cost-f-part2)
                436)

  (check-equal? (cost farm-3 cost-f-part2)
                1206)
  

  )
