#|

Day 1
https://adventofcode.com/2024/day/01

Started this day a bit late since the fact that
it was december 1st completely escaped my mind.

Still getting used to racket so my approach was
parse each line into 2 numbers, then add them to 2 lists
('a' and 'b', where the first number is 'a' and second is 'b')
then sort both list.

For the first part, I fold through the list and subtract the
two numbers and keep a running total.

For the second, I iterate through list 'a' and count
the amount of each item in list 'b', multiple the count by
the number, then keep a running total. There is probably
a much for efficent way to do some count during parsing BUT
its the first day, nothing matters

|#

#lang racket/base

(require racket/string)
(require "../solution.rkt")

(provide run)

;; parse-line :: string  Pair-of-Lists -> Pair-of-Lists
;; will parse the line (a string of two numbers separated by space)
;; and cons the two numbers to the given acculmeator
(define (parse-line l acc)
  (let* ([split (map string->number (string-split l))]
         [listA (car acc)]
         [listB (cdr acc)]
         [itemA (car split)]
         [itemB (cadr split)])
   (cons (cons itemA listA) (cons itemB listB))))

;; parse-input :: string -> Pair-of-Lists-of-Numbers
;; will parse an input of a string with muliptle lines
;; of a pair of numbers separated by string
(define (parse-input i)
  (let* ([lines (string-split i "\n")]
         [p (cons '() '())]
         [parsed (foldl parse-line p lines)]
         [listA (sort (car parsed) <)]
         [listB (sort (cdr parsed) <)])
    (cons listA listB)))

;; get-count :: any list -> integer
;; will count the amount of elements in the list
;;  that are equal to the given item
(define (get-count item l)
  (foldl (λ (i acc)
           (+ acc (if (equal? i item) 1 0)))
         0 l))

(define (run input)
  (let* ([parsed (parse-input input)]
         [listA (car parsed)]
         [listB (cdr parsed)]
         [part1 (foldl (λ (a b acc)
                         (+ (abs (- a b)) acc)) 0 listA listB)]
         [part2 (foldl (λ (a acc)
                         (+ (* a (get-count a listB)) acc))
                       0 listA)])
    (full-solution part1 part2)))


(module+ test
  (require rackunit)
  ;; parse-input test
  (define in #<<EOF
3   4
4   3
2   5
1   3
3   9
3   3
EOF
    )
  (define parsed (parse-input in))
  (check-equal? parsed (cons '(1 2 3 3 3 4) '(3 3 3 4 5 9)))

  ;; parse-line test
  (check-equal? (parse-line "123 456" (cons '() '()))
                (cons '(123) '(456)))

  (check-equal? (parse-line "4 6" (cons '(3 2 1) '(7 8 9)))
                (cons '(4 3 2 1) '(6 7 8 9)))

  ;; run test
  (check-equal? (run in)
                (full-solution 11 31))

  ;; get-count test
  (check-equal? (get-count 2 '(1 1 2 1 1 2 11 3 4 6 2)) 3)
  (check-equal? (get-count 10 '()) 0)
  (check-equal? (get-count 1 '(2 3 4)) 0)
  )
