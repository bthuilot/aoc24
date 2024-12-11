#lang racket/base


(provide

 ;; [<T1> -> <T2>] [Listof <T1>] -> [Listof <T2>]
 map-pair
 )

;; [<T1> -> <T2>] [Listof <T1>] -> [Listof <T2>]
;; applies f to every combination of 2 elements
;; in the given list
(define (map-pair func l)
  (define ((fold i) p acc) (append acc (func p i)))
  (define (helper f ls acc)
    (cond [(<= (length ls) 1) acc]
          [else
           (define applied (foldl (fold (car ls)) '() (cdr ls)))
           (helper f (cdr ls) (append applied acc))]))
  (helper func l '()))