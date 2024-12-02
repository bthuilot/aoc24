#|

Day 2
https://adventofcode.com/2024/day/02

Still getting the hang of using racket so this day took a bit
longer than it should have due to a lot of small mistakes.

The basic plan was parse the input then check
every adjancent pair for each line and see if the
absolute diff was < 4 and > 0. I also recorded the
sign of the diff (positive or negative) and passed that
along to the next pair to check if following diffs are also
in the same direction (increase or decrease).

For part 2, I used the same function but added a 'damepener'
which added an 'or' condition when true that also checked the list
if the current element was skipped. It then recusred with dampener
disabled.

|#

#lang racket/base

(require racket/string)
(require racket/list)

(require "../solution.rkt")


(provide run)


;; parse-levels :: string -> [[integer]]
;; parse day 2 into levels
(define (parse-levels input)
  (let ([lines (string-split input "\n")])
    (map
     (λ (l) (map string->number (string-split l)))
     lines)
    ))

;; a Trend is one of:
;; - 'increase
;; - 'decrease
;; - 'zero

;; same-sign? :: Trend Trend -> boolean
;; checks if two Trendss are the same
;; assues the 'zero trend matches both as it is used
;; as the initial trend
(define (same-sign? new-sign last-sign)
  (or (equal? last-sign 'zero)
      (equal? last-sign new-sign)))

;; get-trend :: integer -> Trend
;; get trends returns the trend for the levels
(define (get-trend diff)
  (cond
    [(equal? diff 0) 'zero]
    [(> diff 0) 'increase]
    [else 'decrease]))

;; report-safe :: [[integer]] boolean? -> boolean
;; determines if a report is safe.
;; optionally can enable the problem-dampener which
;; additionally returns safe reports if removing one
;; level results in a safe report
(define (report-safe? levels #:enable-dampener [problem-dampener #f])
  (letrec ([get-sign (λ (diff) (if (> diff 0) 'increase 'decrease))]
           ;; test is the adjancent is stafe
           [adjacent-safe?
            (λ (diff last-sign next dampener)
              (let ([sign (get-sign diff)]
                    [abs-diff (abs diff)])
                (and (> abs-diff 0) (< abs-diff 4)
                     (same-sign? sign last-sign)
                     (report-safe-helper next sign dampener))))]
           ;; helper, using last-sign and dampener as accumulators
           [report-safe-helper
            (λ (l last-sign dampener)
              (or
               (<= (length l) (if dampener 2 1))
               ;; test without using dampener
               (adjacent-safe?
                ;; diff is l[1] - l[0]
                (- (cadr l) (car l))
                last-sign
                ;; next is l[1:]
                (cdr l)
                dampener)
               ;; if failed, test using dampener if unusued
               (and dampener
                    ;; if we get here, dampener is #t
                    (adjacent-safe?
                     ;; diff is l[2] - l[0]
                     (- (caddr l) (car l))
                     last-sign
                     ;; next is l[2:]
                     (cddr l)
                     ;; dampener is now used
                     #f))))])
    (or
     (report-safe-helper levels 'zero problem-dampener)
     ;; have to also check if using the dampener on the first item
     (and problem-dampener (report-safe-helper (cdr levels) 'zero #f)))))
     



(define (run input)
  (let* ([levels (parse-levels input)]
         [part1 (length (filter report-safe? levels))]
         [part2 (length (filter (λ (l) (report-safe? l #:enable-dampener #t)) levels))])
    (full-solution part1 part2)))


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
  
  (check-equal? (report-safe? '(7 6 4 2 1)) #t)
  (check-equal? (report-safe? '(1 2 7 8 9)) #f)
  (check-equal? (report-safe? '(9 7 6 2 1)) #f)
  (check-equal? (report-safe? '(1 3 2 4 5)) #f)
  (check-equal? (report-safe? '(8 6 4 4 1)) #f)
  (check-equal? (report-safe? '(1 3 6 7 9)) #t)
  
  (check-equal? (report-safe? '(7 6 4 2 1) #:enable-dampener #t) #t)
  (check-equal? (report-safe? '(1 2 7 8 9) #:enable-dampener #t) #f)
  (check-equal? (report-safe? '(9 7 6 2 1) #:enable-dampener #t) #f)
  (check-equal? (report-safe? '(1 3 2 4 5) #:enable-dampener #t) #t)
  (check-equal? (report-safe? '(8 6 4 4 1) #:enable-dampener #t) #t)
  (check-equal? (report-safe? '(1 3 6 7 9) #:enable-dampener #t) #t)

  (check-equal? (report-safe? '(10 8 10 6 4 2 1)) #f)
  (check-equal? (report-safe? '(10 8 10 6 4 2 1) #:enable-dampener #t) #t)
  (check-equal? (report-safe? '(1 2 3 4 5 11) #:enable-dampener #t) #t)
  (check-equal? (report-safe? '(1 2 3 4 5 11)) #f)
  (check-equal? (report-safe? '(99 1 2 3 4 5) #:enable-dampener #t) #t)
  (check-equal? (report-safe? '(99 1 2 3 4 5)) #f)

  (check-equal? (length (filter report-safe? example-levels)) 2)
  (check-equal? (length (filter (λ (l) (report-safe? l #:enable-dampener #t)) example-levels)) 4)
  )
