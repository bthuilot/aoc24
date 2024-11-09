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

