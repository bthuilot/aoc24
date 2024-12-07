#lang racket/base

(require "point.rkt")

(provide
 ;; [Matrixof <T>] Integer Integer -> <T>
 matrix-ref
 ;; [Matrixof <T>] Point -> <T?
 matrix-point)

;; Matrixof<T> = [Listof [Listof T]]
;; Where T is a type

;; Matrix Integer Integer -> Any
;; returns the element of the matrix at (x,y) where y
;; is zero indexed from the top level list and x is
;; zero indexed from the sub lists
(define (matrix-ref m x y)
  (list-ref (list-ref m y) x))

;; Matrix point -> Any
;; Same as 'matrix-ref' but uses a point
;; to specify the x y ref
(define (matrix-point m p)
  (matrix-ref m (point-x p) (point-y p)))