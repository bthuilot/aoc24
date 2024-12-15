#|

Day 14
https://adventofcode.com/2024/day/14

Beginning of the end for me.

The first part was pretty easy,
for each point, add the speed for the respecitve x/y
coordinate multipled by the amount of seconds and
then wrap the coordinate based on the boundary for
the x/y coordinates.

Count each quadarent is just depedning on if the x/y
coordinate is above or below the half way point.

Now, part 2 gave **no hints** so my first idea
was to try to 'wait' for 1 second, check if all points
are connected, if not wait another and so on. That would
not have worked so luckily I browsed reddit to see what
exactly the "christmas tree" grid would look like. I got
lazy and just counted the amount of points that are next
to each other horizontally, then ran the program a few times
changing the 'threshold' for count of adjancent points until
I got my star (it was 100 adjancent points). 

|#

#lang racket/base


(require racket/string)
(require racket/function)
(require "../utils/point.rkt")
(require "../solution.rkt")

(provide run)

;; Regular expression to match a line of input
(define robot-regexp #rx"p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)")


;; (robot Point Slope)
;; represents a robots poisiton and its slope
(struct robot (point speed) #:transparent)

;; Robot = (robot Point Slope)

;; String -> [Listof Robot]
;; parses the input for robots
(define (parse-robots input)
  (define (parse-robot s)
    (define matches (regexp-match* robot-regexp s #:match-select values))
    (define nums (map string->number (cdar matches)))
    (define px (car nums))
    (define py (cadr nums))
    (define vx (caddr nums))
    (define vy (cadddr nums))
    (robot (point px py) (slope vx vy)))

  (define line (string-split input "\n"))
  (map parse-robot line))
  
;; Number Number Number Robot -> Robot
;; Updates a robots position to the point
;; it would teleport after 'seconds' seconds
;; bx and by repesents the bounds of the map
(define (teleport bx by seconds rbot)
  (define (move b c s)
    (define r (remainder (+ c (* s seconds)) b))
    (if (< r 0)
        (- b (abs r))
        r))
  (define p (robot-point rbot))
  (define s (robot-speed rbot))
  (define x (move bx (point-x p) (slope-dx s)))
  (define y (move by (point-y p) (slope-dy s)))
  (robot (point x y) s))

;; Number Number [Listof Robot]
(define (safety-factor bx by robots)
  ; function to be mapped over all points,
  ; updates the poisition to 100 seconds in the future
  (define mapfunc ((curry teleport) bx by 100))
  ; later is all the points after 100 seconds
  (define later (map mapfunc robots))
  ; represents the quadarent counts
  (struct q (ne nw se sw))
  ; half way point for x
  (define half-x (quotient bx 2))
  ; half way point for y
  (define half-y (quotient by 2))
  ; funtion to fold over all 'later' points
  ; updates the counts for each quadarent
  (define (foldfunc r acc)
    (define p (robot-point r))
    (define x-diff (- half-x (point-x p)))
    (define y-diff (- half-y (point-y p)))
    (cond
      ; diff of either is 0, meaning middle point ignore
      [(or (= x-diff 0) (= y-diff 0)) acc]
      ; northeast quadarent
      [(and (> x-diff 0) (> y-diff 0))
       (q (add1 (q-ne acc)) (q-nw acc) (q-se acc) (q-sw acc))
       ]
      ; northwest quadarent
      [(and (< x-diff 0) (> y-diff 0))
       (q (q-ne acc) (add1 (q-nw acc)) (q-se acc) (q-sw acc))
       ]
      ; southeast quadarent
      [(and (> x-diff 0) (< y-diff 0))
       (q (q-ne acc) (q-nw acc) (add1 (q-se acc)) (q-sw acc))
       ]
      ; southwest quadarent
      [else
       (q (q-ne acc) (q-nw acc) (q-se acc) (add1 (q-sw acc)))
       ]))
  (define result (foldl foldfunc (q 0 0 0 0) later))
  ; multiple results together
  (* (q-ne result) (q-nw result) (q-se result) (q-sw result)))


;; Number Number [Listof Robot] -> Number
;; 'waits' one second then checks the amount
;; of horizontally adjancent points. if it is
;; above 100, returns that amount of seconds waited,
;; if not, recurs and increases wait count by 1
(define (easter-egg bx by robots)
  (define (straight-line? cur-r acc)
    (define cout (car acc))
    (define cur-p (robot-point cur-r))
    (define last-p (cdr acc))
    (cons (+ cout (if (and (= (point-x cur-p) (point-x last-p))
                           (= (abs (- (point-y cur-p) (point-y last-p))) 1))
                      1 0))
          cur-p))
  (define mapfunc ((curry teleport) bx by 1))
  (define (find-tree r)
    (define tick (map mapfunc r))
    (define sorted (sort tick (λ (r1 r2)
                                (define p1 (robot-point r1))
                                (define p2 (robot-point r2))
                                (if (= (point-x p1) (point-x p2))
                                    (< (point-y p1) (point-y p2))
                                    (< (point-x p1) (point-x p2)))
                                )))
    (if (> (car (foldl straight-line? (cons 0 (robot-point (car sorted))) (cdr sorted))) 100)
        1
        (+ 1 (find-tree tick)))
    )
  (find-tree robots))

(define (run input)
  (define robots (parse-robots input))
  (full-solution
   (safety-factor 101 103 robots)
   (easter-egg 101 103 robots)))


(module+ test
  (require rackunit)

  (define example-input #<<EOF
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
EOF
    )

  (define robots (parse-robots example-input))

  (check-equal? robots
                `(
                  ,(robot (point 0 4) (slope 3 -3))
                  ,(robot (point 6 3) (slope -1 -3))
                  ,(robot (point 10 3) (slope -1 2))
                  ,(robot (point 2 0) (slope 2 -1))
                  ,(robot (point 0 0) (slope 1 3))
                  ,(robot (point 3 0) (slope -2 -2))
                  ,(robot (point 7 6) (slope -1 -3))
                  ,(robot (point 3 0) (slope -1 -2))
                  ,(robot (point 9 3) (slope 2 3))
                  ,(robot (point 7 3) (slope -1 2))
                  ,(robot (point 2 4) (slope 2 -3))
                  ,(robot (point 9 5) (slope -3 -3))
                  )
   
                )

  (check-equal? (safety-factor 11 7 robots)
                12)
  )