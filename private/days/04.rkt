#|

Day 4
https://adventofcode.com/2024/day/04

One of those days where I was I picked a "normal" langauge.
Any matrix problem is already hard and on top of it I miss read
the questions and thought wordsearch matches didn't all have to be
in the same direction (i.e. can match in a 'zig-zag').

After a learning curve of using matrices in racket
and figuring my misunderstadning I eventually settled
on 2 common functions:

1. a function that iterates through every point in the matrix and calls
a given function that counts that the amount of matches at the point

2. a function that given a list of letters, a starting point and a direction
will check each point from the starting along the direction and return true if
they match.

It then was just creating the two 'count at the point' functions for both
parts using the 2nd function.

|#

#lang racket/base

(require racket/string)
(require racket/list)
(require "../solution.rkt")
(require "../utils/point.rkt")
(require "../utils/matrix.rkt")

(provide run)

;; A wordsearch is a [[characters]]

;; string->wordsearch :: string -> wordsearch
;; Parses a string into a wordsearch. First the string
;; is split into a list by new-lines, then each item
;; is converted into a list of characters
(define (string->wordsearch input)
  (map string->list (string-split input "\n")))

;; A direction is a function that translates a point

;; create-direction :: integer integer -> (point -> point)
;; creates a direction that will translate a point by dx on the
;; x-axis and dy on the y-axis
(define (create-direction dx dy)
  (λ (p) (translate p dx dy)))

;; in-bounds? :: matrix -> point
;; checks if a point is in the bounds of a given matrix
(define (in-bounds? m p)
  (and (> (length m) (point-y p))
       (> (length (car m)) (point-x p))
       (<= 0 (point-x p))
       (<= 0 (point-y p))))

;; word-match :: wordsearch [characters] point direction -> boolean
;; matches the letters in the correct order starting at
;; point p in the given wordsearch and traveling in the
;; given direction
(define (word-match ws letters p direction)
  (or
   (empty? letters)
   (and (in-bounds? ws p)
        (equal? (matrix-ref ws (point-x p) (point-y p))
                (car letters))
        (word-match ws (cdr letters) (direction p) direction))))

;; count-words :: wordsearch (wordsearch point -> integer) -> integer
;; iterates over all points in the wordserach and will call the given
;; words-at-point function for each point and return the sum of all counts
(define (count-words wordsearch words-at-point)
  (letrec ([search (λ (ws p acc)
                     (if (empty? ws) acc
                         (search (cdr ws) (translate p 0 1)
                                 (+ (search-row (car ws) p 0) acc))
                         ))]
           [search-row (λ (row p acc)
                         (if (empty? row) acc
                             (search-row (cdr row) (translate p 1 0) (+ acc (words-at-point wordsearch p)))))])
    (search wordsearch (point 0 0) 0)
    ))

;; all-directions :: [direction]
;; represents all directions that can be taken from a point
(define all-directions `(,(create-direction 0 1)
                         ,(create-direction 1 0)
                         ,(create-direction 1 -1)
                         ,(create-direction -1 1)
                         ,(create-direction 0 -1)
                         ,(create-direction -1 0)
                         ,(create-direction 1 1)
                         ,(create-direction -1 -1)))

;; xmas-at-point :: wordsearch point -> integer
;; returns the amount of 'xmas' counted at the given point in the wordsearch
(define (xmas-at-point ws p)
  (let ([letters (string->list "XMAS")])
    (length (filter (λ (dir) (word-match ws letters p dir)) all-directions))))

;; x-mas-at-point :: wordsearch point -> integer
;; returns the amount of 'x-mass' counted at the given point.
;; will count the point if is the 'A' of the cross
(define (x-mas-at-point ws p)
  (let ([letters (string->list "MAS")])
    (if (and (or (word-match ws letters (translate p 1 1) (create-direction -1 -1))
                 (word-match ws letters (translate p -1 -1) (create-direction 1 1)))
             (or (word-match ws letters (translate p -1 1) (create-direction 1 -1))
                 (word-match ws letters (translate p 1 -1) (create-direction -1 1))))
        1
        0)))


(define (run input)
  (let* ([wordsearch (string->wordsearch input)]
         [part1 (count-words wordsearch xmas-at-point)]
         [part2 (count-words wordsearch x-mas-at-point)])
  (full-solution part1 part2)))


(module+ test
  (require rackunit)
  (define example-input #<<EOF
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
EOF
    )

  (define example-ws (string->wordsearch example-input))
  (check-equal? example-ws
                '((#\M #\M #\M #\S #\X #\X #\M #\A #\S #\M)
                  (#\M #\S #\A #\M #\X #\M #\S #\M #\S #\A )
                  (#\A #\M #\X #\S #\X #\M #\A #\A #\M #\M )
                  (#\M #\S #\A #\M #\A #\S #\M #\S #\M #\X )
                  (#\X #\M #\A #\S #\A #\M #\X #\A #\M #\M )
                  (#\X #\X #\A #\M #\M #\X #\X #\A #\M #\A )
                  (#\S #\M #\S #\M #\S #\A #\S #\X #\S #\S )
                  (#\S #\A #\X #\A #\M #\A #\S #\A #\A #\A )
                  (#\M #\A #\M #\M #\M #\X #\M #\M #\M #\M )
                  (#\M #\X #\M #\X #\A #\X #\M #\A #\S #\X )))

  (check-equal? (count-words example-ws xmas-at-point)
                18)
  (check-equal? (count-words example-ws x-mas-at-point)
                9)
  )
