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
 )

;; a point represents an x y coordinate
(struct point (x y) #:transparent)

;; Point Number NUmber -> Point
;; Translates a point by dx on the x axis
;; and dy on the y axis
(define (translate p dx dy)
  (point (+ (point-x p) dx) (+ (point-y p) dy)))

(define (translate-slope p s)
  (translate p (slope-dx s) (slope-dy s)))

(struct slope (dx dy))

(define (slope-of p1 p2)
  (slope (- (point-x p2) (point-x p1))
         (- (point-y p2) (point-y p1))))

(define (negate-slope s)
  (slope (* -1 (slope-dx s))
         (* -1 (slope-dy s))))

(define (cardinal-points p)
  (list (translate p 1 0)
        (translate p 0 1)
        (translate p -1 0)
        (translate p 0 -1)))