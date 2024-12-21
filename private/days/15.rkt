#|

Day 15
https://adventofcode.com/2024/day/15

whole lotta edge cases.

I did a recursive solution, where the base case
was returning either #f (hit a wall) or a hash map
for new updates, each recursive case if the previous
returned #f, it did as well. If it returned a hash,
add the new pushes.

really no difference between one and two, just the case
where you push a double wide box up from the corner into
another box. (see 'push-conflict')

|#

#lang racket/base


(require racket/string)
(require racket/match)
(require racket/list)
(require racket/set)
(require racket/function)
(require racket/hash)
(require "../solution.rkt")
(require "../utils/matrix.rkt")
(require "../utils/point.rkt")

(provide run)

;; Warehouse = [Hashof Point -> Character]

;; Move is one of
;; - #\^
;; - #\v
;; - #\>
;; - #\<

;; Moves = [Listof Move]

;; String -> (values Warehouse Point Move)
(define (parse-input input)
  (define-values (grid moves)
    (let ([split (string-split input "\n\n")])
      (values (map string->list (string-split (car split) "\n"))
              (filter (λ (c) (not (equal? c #\newline)))
                      (string->list (cadr split))))))
  (define robot (box #f))
  (define warehouse
    (matrix-fold-point (λ (p i acc)
                         (when (equal? i #\@) (set-box! robot p))
                         (hash-set acc p i))
                       (hash)
                       grid))
  (values
   warehouse
   (unbox robot)
   moves))

;; expands the warehouse by a factor of
;; 2 on the x axis
(define (expand-warehouse wh)
  (define robot (box #f))
  (define (expand tile)
    (match tile
      [#\# (values #\# #\#)]
      [#\O (values #\[ #\])]
      [#\. (values #\. #\.)]
      [#\@ (values #\@ #\.)]))
  
  (define (foldf i acc)
    (define-values (k v) (values (car i) (cdr i)))
    (define-values (x y) (unpack-point k))

    (define-values (v1 v2) (expand v))
    (define-values (p1 p2) (values (point (* 2 x) y)
                                   (point (+ 1 (* 2 x)) y)))
    (when (equal? v #\@) (set-box! robot p1))
    (hash-set (hash-set acc p2 v2) p1 v1))

  (values (foldl foldf (hash) (hash->list wh))
          (unbox robot)))

;; next point based on move
(define (next-point cur move)
  (match move
    [#\^ (translate cur 0 -1)]
    [#\v (translate cur 0 1)]
    [#\> (translate cur 1 0)]
    [#\< (translate cur -1 0)]))

;; get the other side of double wide box
(define (other-side-p p char)
  (cond
    [(equal? char #\]) 
     (values #\[ (translate p -1 0))]
    [(equal? char #\[) 
     (values #\] (translate p 1 0))]))

;; have a small issue when pushing
;; a double wide box that the solution
;; is just dont use ',' when another execution
;; branch put another value in
(define (push-conflict k v1 v2)
  (if (equal? v1 #\.) v2 v1))

;; Point Warehouse Move -> Warehouse
(define (move-robot warehouse robot move)
  (define (push wh c p)
    (define next-p (next-point p move))
    (define next (hash-ref wh next-p #\#))
    (cond
      ; hit a wall
      [(equal? next #\#) #f]
      ; specific case of double wide box
      [(and (or (equal? next #\[) (equal? next #\]))
            (or (equal? move #\^) (equal? move #\v)))
       (define-values (other-c other-p) (other-side-p next-p next))
       (define cur (hash next-p c other-p #\.))
       (define-values (recur other-recur) (values (push wh next next-p)
                                                  (push wh other-c other-p)))

       (if (or (not recur) (not other-recur)) #f
           (hash-union cur recur other-recur #:combine/key push-conflict))]
      
      ; normal box or double wide from the side
      [(or (equal? next #\O) (equal? next #\[) (equal? next #\]))
       (define recur (push wh next next-p))
       (if (not recur) #f
           (hash-set recur next-p c))]
      ; Nothin in spot
      [(equal? next #\.) (hash next-p c)]))
  ; push starting from robot
  ; if #f, hit wall. if a hash, the new values to update
  (define res (push warehouse #\@ robot ))
  ; overwrite the robot's current spot
  (if (not res) (cons robot warehouse)
      (cons (next-point robot move)
            (hash-union (hash-set res robot #\.) warehouse
                        ; take values from hash one over existing
                        #:combine/key (lambda (k v1 v2) v1)))))

(define (push-boxes warehouse robot moves)
  (cdr (foldl (λ (m acc)
                (define-values (r wh) (values (car acc) (cdr acc)))
                (move-robot wh r m))
          (cons robot warehouse)
          moves)))

(define (sum-gps wh)
  (define (gps p)
    (+ (* 100 (point-y p)) (point-x p))
    )
  (define (is-wall? i)
    (or (equal? i #\O) (equal? i #\[)))
  (define coords
    (hash-map wh (λ (k v)
                   (if (is-wall? v) (gps k) 0))))
  (apply + coords))

(define (run input)
  (define-values (warehouse robot moves) (parse-input input))
  (define pushed (push-boxes warehouse robot moves))
  (define-values (expanded-wh expanded-r) (expand-warehouse warehouse))
  (define expanded-pushed (push-boxes expanded-wh expanded-r moves))
  (full-solution
   (sum-gps pushed)
   (sum-gps expanded-pushed)))

(module+ test
  (require rackunit)

  (define small-example-input #<<EOF
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
EOF
    )
  (define-values (wh-sm robot-sm moves-sm) (parse-input small-example-input))

  (check-equal? robot-sm (point 2 2))

  (define pushed-sm (push-boxes wh-sm robot-sm moves-sm))
  (check-equal? (sum-gps pushed-sm) 2028)
  
  (define example-input #<<EOF
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
EOF
    )

  (define-values (warehouse robot moves) (parse-input example-input))

  (check-equal? robot (point 4 4))

  (define pushed (push-boxes warehouse robot moves))
  (check-equal? (sum-gps pushed) 10092)


  (define example-expanded #<<EOF
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
EOF
    )
  (define-values (wh-3 robot-3 moves-3) (parse-input example-expanded))
  (define-values (expanded-sm robot-expand-sm) (expand-warehouse wh-3))
  (check-equal? robot-expand-sm
                (point 10 3))
  (define pushed-expanded-sm (push-boxes expanded-sm robot-expand-sm moves-3))
  (check-equal? (sum-gps pushed-expanded-sm)
                (+ 105 207 306))

  (define example-expanded2 #<<EOF
#######
#.....#
#..O..#
#..O..#
#..O..#
#.@...#
#######

>>^
EOF
  )

  (define-values (wh-4 r-4 m-4) (parse-input example-expanded2))
  (define-values (wh-4e r-4e) (expand-warehouse wh-4))
  (define wh-4e-pushed (push-boxes wh-4e r-4e m-4))
  (check-equal? (sum-gps wh-4e-pushed)
                618)




  (define-values (expanded robot-expnded) (expand-warehouse warehouse))
  (define pushed-expanded (push-boxes expanded robot-expnded moves))
  (check-equal? (sum-gps pushed-expanded)
                9021)
  )
