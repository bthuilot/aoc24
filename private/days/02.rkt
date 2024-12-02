#|

Day 2
https://adventofcode.com/2024/day/02

|#

#lang racket/base

(require racket/string)
(require racket/list)

(require "../solution.rkt")


(provide run)


(define (parse-levels input)
  (let ([lines (string-split input "\n")])
    (map
     (λ (l) (map string->number (string-split l)))
     lines)
    ))

(define (same-sign? new-sign old-sign)
  (or (equal? old-sign 'no-sign)
       (equal? old-sign new-sign)))

(define (safe-level? level)
  (letrec ([safe-level
            (λ (l s)
              (cond
                [(empty? l) 0]
                [(equal? (length l) 1) 0]
                [else (let* ([diff (- (cadr l) (car l))]
                             [sign (if (> diff 0) 'increase 'decrease)]
                             [abs-diff (abs diff)])
                        (+ (if (and (< abs-diff 4) (> abs-diff 0)
                                    (same-sign? sign s))
                               0 1)
                           (safe-level (cdr l) sign)))]))])
    (safe-level level 'no-sign)))
         

(define (run input)
  (let* ([levels (parse-levels input)]
         [part1 (length (filter safe-level? levels))]
         [part2 null])
    (part1-solution part1)))


(module+ test
  (require rackunit)
  (define example-input #<<EOF
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
EOF
    )

  (define example-levels (parse-levels example-input))
  (check-equal? example-levels '((7 6 4 2 1)
                                 (1 2 7 8 9)
                                 (9 7 6 2 1)
                                 (1 3 2 4 5)
                                 (8 6 4 4 1)
                                 (1 3 6 7 9)))
  
  (check-equal? (safe-level? '(7 6 4 2 1)) #t)
  (check-equal? (safe-level? '(1 2 7 8 9)) #f)
  (check-equal? (safe-level? '(9 7 6 2 1)) #f)
  (check-equal? (safe-level? '(1 3 2 4 5)) #f)
  (check-equal? (safe-level? '(8 6 4 4 1)) #f)
  (check-equal? (safe-level? '(1 3 6 7 9)) #t)
  )
