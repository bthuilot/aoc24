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
 cardinal-points

 ;; Direction is one of
 ;; 'north
 ;; 'south
 ;; 'east
 ;; 'west

 ;; Point -> [Listof [Pairof Point Direction]]
 cardinal-points-dir
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
;; returns all the cardinally surrounding
;; points of the given point 
(define (cardinal-points p)
  (list (translate p 1 0)
        (translate p 0 1)
        (translate p -1 0)
        (translate p 0 -1)))

(define (cardinal-points-dir p)
  (list (cons (translate p 1 0) 'east)
        (cons (translate p 0 1) 'south)
        (cons (translate p -1 0) 'west
              )
        (cons (translate p 0 -1) 'north)))

