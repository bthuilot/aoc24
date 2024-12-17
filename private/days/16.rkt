#|

Day 16
https://adventofcode.com/2024/day/16

I call this one "im done with matrix problems" so
here is a bunch of spaghetti code I will not be cleaning up.

I just did "dijkstras", or rather whatever I remember from
algorithms class slopply through together

|#

#lang racket/base

(require racket/string)
(require racket/match)
(require racket/list)
(require racket/set)
(require "../solution.rkt")
(require "../utils/matrix.rkt")
(require "../utils/point.rkt")

(provide run)

(define (parse-maze input)
  (define lines (string-split input "\n"))
  (map string->list lines))

(define (find-start maze)
  (define (fold-func p i acc)
    (cond
      [(equal? i #\S) p]
      [else acc]))
  (matrix-fold-point fold-func #f maze))

(define (turn-180 dir)
  (match dir
    ['north 'south]
    ['south 'north]
    ['east 'west]
    ['west 'east]))

(define (traverse-maze start maze)
  (struct next (point dir score))
  (define h (make-hash))
  (define ((onto-next paths) ns seen)
    (cond
      [(empty? ns) seen]
      [else
       (define n (car ns))
       (define p (next-point n))
       (define d (next-dir n))
       (define s (next-score n))
       (define k (pos p d))
       (cond
         [(and (hash-has-key? seen k) (< (hash-ref seen k) s))
          (onto-next (cdr ns) seen)]
         [else
          ((onto-next paths) (cdr ns) (traverse seen s (cons p paths) k))]
         )])
    )

  (define (add-to-seen n seen)
    (hash-set* seen (pos (next-point n) (next-dir n)) (next-score n)))
  

  (define ((collect-next seen cur-score facing) cardinal a)
    (define one80 (turn-180 facing))
    (define next-p (car cardinal))
    (define next-dir (cdr cardinal))
    (cond
      [(equal? one80 next-dir) a]
      [(equal? (matrix-point maze next-p) #\#) a]
      [else
       (define score 
         (+ cur-score 1 (cond
                          [(equal? next-dir facing) 0]
                          [else 1000])))
       
       (if (or (not (hash-has-key? seen (pos next-p next-dir)))
               (>= (hash-ref seen (pos next-p next-dir)) score)
               )
           (cons (next next-p next-dir score) a)
           a)]))
  
  (define (traverse seen score path position)
    (define p (pos-point position))
    (define d (pos-dir position))
    (define tile (matrix-point maze p))
    (cond
      [(equal? tile #\E)
       ; if the tile is e, just put the score in
       ; the visited map as "E" lol
       (if (or (not (hash-has-key? seen tile)) (<= score (hash-ref seen tile)))
           (begin
             (hash-set*! h score (set-union (list->set path) (hash-ref h score set)))
             (hash-set seen tile score)
             )
           seen)
       ]
      [(equal? tile #\#)  seen]
      [else
       (define nexts (foldl (collect-next seen score d) '() (cardinal-points-dir p)))
       (define new-seen (foldl add-to-seen seen nexts))
       ((onto-next path)
        (sort nexts (λ (n1 n2) (< (next-score n1) (next-score n2))))
        new-seen)

       ]))
  (struct pos (point dir) #:transparent)
  (define start-pos (pos start 'east))
  (define traversed (traverse (hash start-pos 0) 0 `(,start)  start-pos))
  ; record end as special "E"
  (define score (hash-ref traversed #\E))
  (define sorted-keys (apply min (hash-keys h)))
 
  (values score (set-count (hash-ref h score))))

(define (run input)
  (define maze (parse-maze input))
  (define start (find-start maze))
  (define-values (lowest-score c) (traverse-maze start maze))
  (full-solution
   lowest-score
   c
   )
  )


(module+ test
 (require rackunit)

 (define example-input #<<EOF
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
EOF
)

 (define example-input-2 #<<EOF
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
EOF
)

 (define maze1 (parse-maze example-input))
 (define start1 (find-start maze1))
 (check-equal? start1
               (point 1 13))

 
 (define-values (l1 c1) (traverse-maze start1 maze1))
 (check-equal? l1 7036)
 (check-equal? c1 45)

 (define maze2 (parse-maze example-input-2))
 (define start2 (find-start maze2))
 (define-values (l2 c2) (traverse-maze start2 maze2))
 (check-equal? l2 11048)

 (check-equal? c2 64)
 
 )

