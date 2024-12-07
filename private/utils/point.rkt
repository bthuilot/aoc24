#lang racket/base


(provide
 ;; (point Number Number)
 (struct-out point)
 
 ;; point Number Number -> point
 translate)

;; a point represents an x y coordinate
(struct point (x y) #:transparent)

(define (translate p dx dy)
  (point (+ (point-x p) dx) (+ (point-y p) dy)))