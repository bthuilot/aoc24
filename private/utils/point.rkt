#lang racket/base

(provide
 ;; Point = (point Number Number)
 
 ;; (point Number Number)
 (struct-out point)

 
 ;; Point Number Number -> Point
 translate

 ;; Slope = (slop Number Number)
 ;; (slope Number Number)
 (struct-out slope)
 
 ;; Point Point -> Slope
 slope-of

 ;; Slope -> Slope
 negate-slope

 ;; Point Slope -> Point
 translate-slope

 ;; Point -> [Listof Point]
 ordinal-points

 ;; Point -> [Listof Point]
 cardinal-points
 
 ;; Point -> [Listof Point]
 surrounding-points

 ;; Direction is one of
 ;; 'north
 ;; 'south
 ;; 'east
 ;; 'west

 ;; Point -> [Listof [Pairof Point Direction]]
 cardinal-points-dir

 ;; Point -> String
 point->string

 ;; Point Number -> [Listof Point]
 manhattan-distance-points

 ;; Point -> (values Number Number)
 unpack-point
 )

;; a point represents an x y coordinate
(struct point (x y) #:transparent)

;; Point Number Number -> Point
;; Translates a point by dx on the x axis
;; and dy on the y axis
(define (translate p dx dy)
  (point (+ (point-x p) dx) (+ (point-y p) dy)))

;; Point Slope -> Point
;; same as 'translate' but uses the slope
;; for dx and dy
(define (translate-slope p s)
  (translate p (slope-dx s) (slope-dy s)))

;; (slope Number Number)
;; represents the slope of a line
;; or change in point
(struct slope (dx dy) #:transparent)

;; Point Point -> Slope
;; Calculates the slope of two points
(define (slope-of p1 p2)
  (slope (- (point-x p2) (point-x p1))
         (- (point-y p2) (point-y p1))))

;; Slope -> Slope
;; negates a slope 
(define (negate-slope s)
  (slope (* -1 (slope-dx s))
         (* -1 (slope-dy s))))


;; Point -> [Listof Point]
;; returns all the surrounding
;; points of the given point
;; both cardinally and ordinally
(define (surrounding-points p)
  (append (cardinal-points p)
          (ordinal-points p)))

;; Point -> [Listof Point]
;; returns all the cardinally surrounding
;; points of the given point
(define (cardinal-points p)
  (list (translate p 1 0)
        (translate p 0 1)
        (translate p -1 0)
        (translate p 0 -1)))

;; Point -> [Listof Point]
;; returns all the ordinally surrounding
;; points of the given point
(define (ordinal-points p)
  (list (translate p 1 1)
        (translate p -1 -1)
        (translate p -1 1)
        (translate p 1 -1)))

;; Point -> [Listof [Pairof Point Direction]]
;; returns all the cardinally surrounding
;; points and their direction
(define (cardinal-points-dir p)
  (list (cons (translate p 1 0) 'east)
        (cons (translate p 0 1) 'south)
        (cons (translate p -1 0) 'west
              )
        (cons (translate p 0 -1) 'north)))

(define (unpack-point p)
  (values (point-x p) (point-y p)))

(define (manhattan-distance-points p dist)
  (define-values (x y) (unpack-point p))
  (apply append
         (build-list dist
                     (λ (offset)
                       (define inv-offset (- dist offset))
                       (list (point (+ x offset) (+ y inv-offset))
                             (point (+ x inv-offset) (- y offset))
                             (point (- x offset) (- y inv-offset))
                             (point (- x inv-offset) (+ y offset))
                             )))))
;; Point -> String
;; formats string as x,y
(define (point->string p)
  (format "(~a,~a)" (point-x p) (point-y p)))

(module+ test
  (require rackunit)

  (check-equal? (manhattan-distance-points (point 3 3) 3)
                (list
                 (point 3 6)
                 (point 6 3)
                 (point 3 0)
                 (point 0 3)
                 (point 4 5)
                 (point 5 2)
                 (point 2 1)
                 (point 1 4)
                 (point 5 4)
                 (point 4 1)
                 (point 1 2)
                 (point 2 5)))
  )