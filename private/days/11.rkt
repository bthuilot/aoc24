#|

Day 11
https://adventofcode.com/2024/day/11

This day was really easy in a functional
language, just map over the input with a
'blink-stone' function.

However, part 2 ends up running for a
loonnnnggggg time. At first I re-wrote
mine to be tail recursive to hopefully
save resources to no avail.

after running it for > 3 hours I decided
to try to "slightly" improve it by adding
a cache of previously computed results.

Ran in 2 seconds :/

|#

#lang racket/base


(require racket/string)
(require racket/function)
(require "../solution.rkt")

(provide run)

;; Stone = Number

;; Stones = [Listof Stone]

;; String -> Stones
;; parses a string of numbers seperated by
;; space into a Stones
(define (parse-stones input)
  (map string->number (string-split input)))

;; splits a stone string in half
;; to a values of the first and second half
;; e.g. (partition-stone "1234") -> (values 12 34)
(define (partition-stone str)
  (define l (string-length str))
  (define half-point (/ l 2))
  (define first-half (substring str 0 half-point))
  (define second-half (substring str half-point))
  (values (string->number first-half)
          (string->number second-half)))

;; Number -> [Listof Number]
;; returns the list of stones that result
;; from blinking once with the given stone
(define (blink-stone stone)
  (define stone-str (number->string stone))
  (cond
    [(= 0 stone) '(1)]
    [(even? (string-length stone-str))
     (define-values (f s) (partition-stone stone-str))
     `(,f ,s)]
    [else `(,(* 2024 stone))]))

;; Stones Number -> Number
;; returns the amount of stones that are present
;; after the given number of blinks
(define (count-stones-after stones blinks)
  ;; stores a pair of Number Number mapped to a Number
  ;; which present the stone number and amount of blinks
  ;; mapped to the stone count after that many blinks
  (define cache (make-hash))
  ;; Number Number -> Number
  (define (count-stones stone blinks)
    (define key (cons stone blinks))
    (cond
      [(= blinks 0) 1]
      [(hash-has-key? cache key) (hash-ref cache key)]
      [else
       ;; blinked is the 'stones produced from 'stone'
       ;; after a blink
       (define blinked (blink-stone stone))
       ;; the total count of all stones 
       (define stone-count (count-all-stones (sub1 blinks) blinked))
       ;; update cache
       (hash-set*! cache (cons stone blinks) stone-count)
       ;; return stone count
       stone-count
       ]))
  ;; performs 'count-stones' on all stones in a list
  ;; and sums the results the sum
  (define (count-all-stones b s)
    (apply + (map (λ (i) (count-stones i b)) s)))
  (count-all-stones blinks stones))


(define (run input)
  (define stones (parse-stones input))
  (define part1 (count-stones-after stones 25))
  (define part2 (count-stones-after stones 75))
  (full-solution part1 part2))


(module+ test
  (require rackunit)

  (define example-input "125 17")

  (define stones (parse-stones example-input))

  (check-equal? stones
                '(125 17))

  (check-equal? (blink-stone 125)
                '(253000))
  (check-equal? (blink-stone 17)
                '(1 7))
  
  (check-equal? (count-stones-after stones 25)
                55312)
  )
