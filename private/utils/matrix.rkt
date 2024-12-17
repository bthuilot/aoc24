#lang racket/base

(require racket/list)
(require "point.rkt")

(provide
 ;; [Matrixof <T>] Integer Integer -> <T>
 matrix-ref
 ;; [Matrixof <T>] Point -> <T?
 matrix-point
 ;; [Matrixof <T>] Point -> Boolean
 in-bounds?
 ;; Point Number Number (Number)? (Number)? -> Boolean
 point-within?

 ;; [<A> <T> -> <A>] <A> [Matrixof <T>] -> <A>
 matrix-fold

 ;; [Point <T> <A> -> <A>] <A> [Matrixof <T>] -> <A>
 matrix-fold-point
 )

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

;; Matrix Point -> Boolean
;; returns true if the point is contained
;; within the bounds of the matrix
(define (in-bounds? m p)
  (and (>= (point-x p) 0)
       (>= (point-y p) 0)
       (< (point-x p) (length (car m)))
       (< (point-y p) (length m))))

;; Point Number Number (Number)? (Number)? -> Boolean
;; determines if a point is below a given
;; max x and y, and optionally above a
;; minimum (default is 0)
(define (point-within?
         p max-x max-y
         #:min-x [min-x 0]
         #:min-y [min-y 0])
  (and (>= (point-x p) min-x)
       (>= (point-y p) min-y)
       (< (point-x p) max-x)
       (< (point-y p) max-y)))

;; [<A> <T> -> <A>] <A> [Matrixof <T>] -> <A>
;; folds over all values in a matrix.
(define (matrix-fold f acc matrix)
  (define (fold-row row acc)
    (foldl f acc row))
  (define (fold-rows rows acc)
    (foldl fold-row rows))
  (foldl fold-rows acc matrix))


;; [Point <T> <A> -> <A>] <A> [Matrixof <T>] -> <A>
;; folds over all values in a matrix.
;; Same as matrix-fold but additionally calls
;; with a Point representing the coordinate in the matrix
(define (matrix-fold-point f acc matrix)
  (define (fold-row rs p acc)
    (define next-point (point (add1 (point-x p)) (point-y p)))
    (define (fold i) (f p i acc))
    (cond
      [(empty? rs) acc]
      [else (fold-row (cdr rs) next-point (fold (car rs)))]))
  (define (fold-rows m p acc)
    (define next-point (point (point-x p) (add1 (point-y p))))
    (define (fold i) (fold-row i p acc))
    (cond
      [(empty? m) acc]
      [else (fold-rows (cdr m) next-point (fold (car m)))]))
  (fold-rows matrix (point 0 0) acc))
