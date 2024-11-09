#|

Day 0: Template

This file serves as a template for the daily problems. It is a simple
Racket file that defines a module with a single function, `run`, that
takes a string as input and returns a cons pair of two strings. The
first string is the solution to the first part of the problem, and the
second string is the solution to the second part of the problem.

|#

#lang racket/base


(require racket/string)

(provide run)

  
;; Run: string -> cons
;; Run the solution to the problem
(define (run input)
  (let ([part1 input]
        [part2 (string-upcase input)])
    (cons part1 part2)))

(module+ test
  (require rackunit)
  
  (check-equal? (run "input") (cons "input" "INPUT")))

