#lang racket/base

; Provides
(provide solution->string)
(provide full-solution)
(provide part1-solution)
(provide part2-solution)

; A Solution is one of:
; - (make-full-solution String String)
; - (make-part1-solution String)
; - (make-part2-solution String)
; Interpretation:
;; - A full-solution is a pair of strings, the first representing the solution to part 1 and the second representing the solution to part 2.
(struct full-solution (part1 part2) #:transparent)
;; - A part1-solution is a string representing the solution to part 1.
(struct part1-solution (result) #:transparent)
;; - A part2-solution is a string representing the solution to part 2.
(struct part2-solution (result) #:transparent)


(define (solution->string solution)
  (cond
    [(full-solution? solution)
     (format "part 1: ~a\npart 2: ~a" (full-solution-part1 solution) (full-solution-part2 solution))]
    [(part1-solution? solution)
     (format "part 1: ~a\npart 2: not implemented" (part1-solution-result solution))]
    [(part2-solution? solution)
     (format "part 1: not implemented\npart 2: ~a" (part2-solution-result solution))]
    [else "not implemented"]))



