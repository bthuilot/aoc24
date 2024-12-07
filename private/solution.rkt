#lang racket/base

; Provides
(provide
 ;; Solution -> String
 solution->string
 ;; (full-solution Any Any)
 full-solution
 ;; (part1-solution String)
 part1-solution
 ;; (part2-solution String)
 
 part2-solution)


;; Solution = 
;; (full-solution Any Any) |
;; (part1-solution Any) |
;; (part2-solution Any)


;; Represents the full solution to problem
(struct full-solution (part1 part2) #:transparent)
;; Represents a solution to only part 1 of the problem
(struct part1-solution (result) #:transparent)
;; Represents a solution to only part 2 of the problem
(struct part2-solution (result) #:transparent)

;; Solution -> String
;; Formats the solution output to a human readable string
(define (solution->string solution)
  (cond
    [(full-solution? solution)
     (format "part 1: ~a\npart 2: ~a" (full-solution-part1 solution) (full-solution-part2 solution))]
    [(part1-solution? solution)
     (format "part 1: ~a\npart 2: not implemented" (part1-solution-result solution))]
    [(part2-solution? solution)
     (format "part 1: not implemented\npart 2: ~a" (part2-solution-result solution))]
    [else "not implemented"]))



