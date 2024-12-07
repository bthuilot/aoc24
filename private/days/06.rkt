#|

Day 6
https://adventofcode.com/2024/day/06

This day took me till 11:30 and its only day 6...

I started off strong and got part 1 pretty easily.
Traverse the lab starting from the guard positions,
facing north. I record the direction im facing and
make a function that can take a point and a direction
and return the next step I would take. I then contiually
compute the next step until either reaching a border or
hitting a '#' where I update the direction to 90 degrees
to the right. I record every point in set and count the set
length at the end.

For part 2, I came up with traversing the lab the same way,
but at each point, if im not about to cross the lab border
nor is there a '#' on the next step, run a function to check 'what-if'
placing a '#' in front of me would cause a loop (i.e. continually
run until reaching an end condition meaning no loop, or end up
on a visited tile in the same direction). I then just record which
'what-if' points resulted in a loop and count them.

HOWEVER, I missed the edge case were if a make a 'what-if' point
on a tile that I already traversed, it techinically wouldnt work
since I wouldnt have gotten to the current point if it was blocked.

2 hours wasted on that

|#

#lang racket/base

(require racket/string)
(require racket/list)
(require racket/set)
(require racket/match)
(require "../solution.rkt")
(require "../utils/point.rkt")
(require "../utils/matrix.rkt")

(provide run)

;; a visited-map is a set of visited points
;; represneted by a hash table that maps points
;; to a set of directions that the guard traversed
;; the point facing. an empty set or key missing
;; indicates the guard did not traverse it

;; mark-visited :: visited-map point direction -> visited-map
(define (mark-visited visited point dir)
  (hash-set visited point
            (set-add (hash-ref visited point set) dir)))

;; visited-facing? :: visited-map point direction -> boolean
;; checks if the point has already been visited in the same direction
(define (visited-facing? visited point dir)
  (set-member? (hash-ref visited point set) dir))

;; a lab is a [List-of [List-of character]]

;; parse-lab :: string -> lab
(define (parse-lab input)
  (map string->list (string-split input "\n")))

;; a direction is one off
;; - 'north
;; - 'south
;; - 'east
;; - 'west

;; turn :: direction -> direction
;; turns the direction 90 degrees
;; to the right
(define (turn direction)
  (match direction
    ['north 'east]
    ['east 'south]
    ['south 'west]
    ['west 'north]))

;; travel :: point -> direction
;; translates a point by 1 unit in the direction
(define (travel p dir)
  (match dir
    ['north (translate p 0 -1)]
    ['east (translate p 1 0 )]
    ['south (translate p 0 1)]
    ['west (translate p -1 0)]))

;; in-bounds? [List-of [List-of any]] point -> boolean
(define (in-bounds? m p)
  (and (>= (point-x p) 0)
       (< (point-x p) (length (car m)))
       (>= (point-y p) 0)
       (< (point-y p) (length m))))

;; find-guard :: lab -> point
(define (find-guard lab)
  (letrec (;; searches each row
           ;; it returns a point if it finds the guard or #f otherwise
           [find-guard-row (λ (r p)
                             (cond
                               [(empty? r) #f]
                               [(equal? (car r) #\^) p]
                               [else (find-guard-row (cdr r) (translate p 1 0))]))]
           ;; iterates through row, calling 'find-guard-row'
           [find-guard (λ (l p)
                         (if (empty? l) (error "no guard found")
                             (let ([found-row (find-guard-row (car l) p)])
                               (if (point? found-row) found-row
                                   (find-guard (cdr l) (translate p 0 1))))))])
    (find-guard lab (point 0 0))))

;; guard-path :: lab point -> [Set-of point]
;; traverses the path the guard would take from 'start' point
;; and returns a set of points that they would take
(define (guard-path lab start)
  (letrec (;; returns true of the point is a wall
           [is-wall? (λ (p) (equal? (matrix-point lab p) #\#))]
           ;; step takes the next step for the guard in the direction
           [step (λ (cur visited direction)
                   (let ([next (travel cur direction)]
                         [turned (turn direction)])
                     (cond
                       [(not (in-bounds? lab next)) visited]
                       [(is-wall? next) (step cur visited turned)]
                       [else (step next (set-add visited next) direction)])))])
    (step start (set start) 'north)))


;; possible-objstruction-loops:: lab point -> [Set-of points]
;; finds all points that if an objstruction was placed
;; would cause a loop
(define (possible-obstruction-loops lab start)
  (letrec (;; returns true if the point is a wall
           [is-wall? (λ (p) (equal? (matrix-point lab p) #\#))]
           ;; returns true if traversing from point 'p' in direction 'dir'
           ;; results in a loop by adding a wall at point 'added-block'
           [is-loop? (λ (added-block seen p dir)
                       (let ([next (travel p dir)]
                             [updated-seen (mark-visited seen p dir)])
                         (cond
                           ;; already was here in same direction -> loop 
                           [(visited-facing? seen p dir) #t]
                           ;; next is out of bounds -> no loop
                           [(not (in-bounds? lab next)) #f]
                           ;; if the next block is a wall or the new block -> turn then recur to check loop
                           [(or (equal? added-block next) (is-wall? next)) (is-loop? added-block updated-seen p (turn dir))]
                           ;; otherwise -> recur to check loop
                           [else (is-loop? added-block updated-seen (travel p dir) dir)]))
                       )]
           ;; step first checks if the next step were a wall, would a loop be formed
           ;; and records that point in the 'loops' if true (set of all possible block points)
           ;; then it recurs, taking the next step for guard
           [step (λ (cur visited direction loops)
                   (let ([next (travel cur direction)]
                         [turned (turn direction)])
                     (cond
                       ;; out of bounds -> over
                       [(not (in-bounds? lab next)) loops]
                       ;; next is a wall -> turn
                       [(is-wall? next) (step cur (mark-visited visited cur turned) turned loops)]
                       [else
                        ;; otherwise recur
                        (step next (mark-visited visited next direction) direction
                              ;; update the 'loops' by checking if next point was a block
                              ;; would we form loop
                              (if (and
                                   ;; if the next is already a loop or we visited it, skip
                                   (not (set-member? loops next)) (not (hash-has-key? visited next))
                                   ;; check if next is a loop
                                   (is-loop? next visited cur (turn direction)))
                                  ;; if it is, record it
                                  (set-add loops next)
                                  loops))])))])
    (step start (hash start (set 'north)) 'north (set))))

(define (run input)
  (let* ([lab (parse-lab input)]
         [guard (find-guard lab)]
         [part1 (set-count (guard-path lab guard))]
         [part2 (set-count (possible-obstruction-loops lab guard))])
    (full-solution part1 part2)))


(module+ test
  (require rackunit)
  (define example-input #<<EOF
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
EOF
    )

  (define example-lab (parse-lab example-input))
  (define example-point (find-guard example-lab))

  (check-equal? example-lab
                '((#\. #\. #\. #\. #\# #\. #\. #\. #\. #\. )
                  (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\# )
                  (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\. )
                  (#\. #\. #\# #\. #\. #\. #\. #\. #\. #\. )
                  (#\. #\. #\. #\. #\. #\. #\. #\# #\. #\. )
                  (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\. )
                  (#\. #\# #\. #\. #\^ #\. #\. #\. #\. #\. )
                  (#\. #\. #\. #\. #\. #\. #\. #\. #\# #\. )
                  (#\# #\. #\. #\. #\. #\. #\. #\. #\. #\. )
                  (#\. #\. #\. #\. #\. #\. #\# #\. #\. #\. )))

  (check-equal? example-point
                (point 4 6))
  (define path (guard-path example-lab example-point))
  (check-equal? (set-count path)
                41)
  
  (define loops (possible-obstruction-loops example-lab example-point))
  (check-equal? (set-count loops)
                6)

  (check-equal? loops
                (set (point 3 6) (point 6 7) (point 7 7) (point 1 8) (point 3 8) (point 7 9)))

  (define test-lab '((#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                     (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                     (#\. #\. #\# #\. #\# #\. #\. #\. #\. #\.)
                     (#\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
                     (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                     (#\. #\. #\. #\. #\^ #\. #\. #\. #\. #\.)
                     (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                     (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                     (#\. #\. #\. #\. #\. #\. #\# #\. #\. #\.)
                     (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)))
  (check-equal? (possible-obstruction-loops test-lab (point 4 5))
                (set (point 3 7) (point 1 7)))

  (define test-lab2 '((#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                      (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                      (#\. #\# #\. #\. #\# #\. #\. #\. #\. #\.)
                      (#\. #\. #\. #\# #\. #\. #\. #\# #\. #\.)
                      (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                      (#\. #\. #\. #\. #\^ #\. #\. #\. #\. #\.)
                      (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                      (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                      (#\. #\. #\. #\. #\. #\. #\# #\. #\. #\.)
                      (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)))
   (check-equal? (possible-obstruction-loops test-lab2 (point 4 5))
                 (set (point 3 7) (point 6 4)))

   (define test-lab3 '((#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                       (#\. #\. #\. #\# #\. #\. #\. #\. #\. #\.)
                       (#\. #\# #\. #\. #\. #\. #\. #\. #\. #\.)
                       (#\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
                       (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                       (#\. #\. #\. #\. #\^ #\. #\. #\. #\. #\.)
                       (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                       (#\. #\. #\# #\. #\. #\. #\. #\. #\. #\.)
                       (#\. #\. #\. #\# #\. #\. #\# #\. #\. #\.)
                       (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)))
      (check-equal? (possible-obstruction-loops test-lab3 (point 4 5))
                    (set (point 4 2)))

      (define test-lab4 '((#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                          (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                          (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                          (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                          (#\. #\. #\. #\# #\. #\. #\. #\. #\. #\.)
                          (#\. #\. #\. #\. #\^ #\. #\# #\. #\. #\.)
                          (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                          (#\. #\. #\# #\. #\. #\. #\. #\. #\. #\.)
                          (#\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
                          (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)))
      (check-equal? (possible-obstruction-loops test-lab4 (point 4 5))
                    (set (point 4 4))
                    )
      )
