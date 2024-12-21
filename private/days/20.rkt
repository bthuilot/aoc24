#|

Day 20
https://adventofcode.com/2024/day/20

Went through a lot of iterations on this one.

The cliff notes is that i eventually realized
that there is only one path so no need to do
any crazy search.

Next, while iterating through I find the shortcuts.
From each point, find all non-wall points that are
a least 'cheat seconds' away by manhattan distance.

The 'seconds' saved is the cost of taking the
shortcut, how far away the shortcut start and end are,
subtracted from how far away the start and end without
taking the shortcut. That can be found by substracting how
far both points are from the start, which is found during
the initial search.

|#

#lang racket/base


(require racket/string)
(require racket/set)
(require racket/list)
(require racket/function)
(require "../utils/matrix.rkt")
(require "../utils/point.rkt")
(require "../utils/priority_queue.rkt")
(require "../solution.rkt")

(provide run)

;; CPU = (cpu [Setof Point] Point Point Point)
(struct cpu (walls start end size))

;; Cheat = (cpu Point Point NUmber)
(struct cheat (start end distance) #:transparent) 

;; String -> CPU
(define (parse-cpu input)
  (define m (map string->list (string-split input "\n")))
  (define start (box #f))
  (define end (box #f))
  (define walls
    (matrix-fold-point
     (λ (p i acc)
       (cond
         [(equal? i #\S) (set-box! start p) acc]
         [(equal? i #\E) (set-box! end p) acc]
         [(equal? i #\#) (set-add acc p)]
         [else acc]))
     (set) m))
  (cpu walls (unbox start) (unbox end)
       (point (length (car m)) (length m))))

;; Cheat [Hash Point -> Number] -> Number
(define (calculate-saved shortcut seen)
  (define start (cheat-start shortcut))
  (define end (cheat-end shortcut))
  (define dist (cheat-distance shortcut))
  (define start-steps (hash-ref seen start))
  (define end-steps (hash-ref seen end))
  (- (- end-steps start-steps) dist))

;; CPU Number -> [Hashof Number -> Number] 
(define (find-shortcuts cpu [cheat-seconds 2])
  (define-values (max-x max-y)
    (values (point-x (cpu-size cpu)) (point-y (cpu-size cpu))))

  ; is the point in bounds, not seen and not a wall
  (define (valid? seen p)
    (and (point-within? p max-x max-y)
         (not (hash-has-key? seen p))
         (not (set-member? (cpu-walls cpu) p))))

  ; returns the next point in the path
  ; should always be 1 and will error if not
  (define (next-point seen p)
    (define next-points
      (filter ((curry valid?) seen)
              (cardinal-points p)))
    (when (= (length next-points) 0)
      (error "no path to end"))
    (when (> (length next-points) 1)
      (error "multiple paths to the end"))
    (car next-points)
    )

  ; finds all possible cheats from the given
  ; point, no more than 'seconds' away
  (define (find-cheats seen point seconds)
    (apply
     append (build-list
             (add1 seconds)
             (λ (d)
               ; get all points reached after 'seconds'
               (define m (manhattan-distance-points point d))
               ; remove out of bounds and walls
               (define ends (filter (curry valid? seen) m))
               ; turn them into cheats
               (map (λ (e) (cheat point e d)) ends)))))


  ; traverses the map for all shortcuts
  (define (traverse shortcuts steps seen p)
    (define new-seen (hash-set seen p steps))
    (cond
      [(equal? p (cpu-end cpu)) (values shortcuts new-seen)]
      [else
       (define next (next-point seen p))
       (define new-shortcuts
         (append shortcuts
                 (find-cheats new-seen p cheat-seconds)))
       (traverse new-shortcuts (add1 steps) new-seen next)]
      ))

  (define-values (shortcuts seen)
    (traverse '() 0 (hash) (cpu-start cpu)))
  
  (foldl (λ (cheat acc)
           (define saved (calculate-saved cheat seen))
           (hash-set acc saved (add1 (hash-ref acc saved 0))))
         (hash)
         shortcuts))

;; [Hashof Number -> Number] (Number Number -> Number) -> Number
(define (hash-sum shortcuts f)
  (apply + (hash-map shortcuts f)))


(define (run input)
  (define cpu (parse-cpu input))
  (define-values (start end size)
    (values (cpu-start cpu) (cpu-end cpu) (cpu-size cpu)))
  (define shortcuts (find-shortcuts cpu))
  (define long-shortcuts (find-shortcuts cpu 20))
  (full-solution
   (hash-sum shortcuts (λ (s c) (if (>= s 100) c 0)))
   (hash-sum long-shortcuts (λ (s c) (if (>= s 100) c 0)))))

(module+ test
 (require rackunit)

 (define example-input #<<EOF
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
EOF
)

 (define cpu (parse-cpu example-input))

 (check-equal? (cpu-start cpu)
               (point 1 3))

 (check-equal? (cpu-end cpu)
               (point 5 7))

 (check-equal? (cpu-size cpu)
               (point 15 15))


 (define shortcuts (find-shortcuts cpu))

 (check-equal? (hash-ref shortcuts 2) 14)
 (check-equal? (hash-ref shortcuts 4) 14)
 (check-equal? (hash-ref shortcuts 6) 2)
 (check-equal? (hash-ref shortcuts 8) 4)
 (check-equal? (hash-ref shortcuts 10) 2)
 (check-equal? (hash-ref shortcuts 12) 3)
 (check-equal? (hash-ref shortcuts 20) 1)
 (check-equal? (hash-ref shortcuts 36) 1)
 (check-equal? (hash-ref shortcuts 38) 1)
 (check-equal? (hash-ref shortcuts 40) 1)
 (check-equal? (hash-ref shortcuts 64) 1)

 (define long-shortcuts (find-shortcuts cpu 20))

 (check-equal? (hash-ref long-shortcuts 64) 19)
 
 (check-equal? (hash-ref long-shortcuts 76) 3)

 
 )
