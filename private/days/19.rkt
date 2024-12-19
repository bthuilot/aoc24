#|

Day 19
https://adventofcode.com/2024/day/19

Part 1 was simplying building a state machine.
I implemented it by reading one character at a time
and seeing if i had a match. I would recur on both
(if it had match) removing the match and recuring on the rest
and increases the substring by 1 and recuring.

Currently part 2 is brute force-esque, but
gonna spend some time catching up today

|#

#lang racket/base

(require racket/set)
(require racket/string)
(require racket/function)
(require "../solution.rkt")


(provide run)

;; String -> (values [Listof String] [Listof String])
(define (parse-input input)
  (define split (string-split input "\n\n"))
  (define towels (string-split (car split) ", "))
  (define designs (string-split (cadr split) "\n"))

  (values towels designs))

;; [Setof String] [Listof String] -> Boolean
;; returns true if the design can made from
;; towels in the set
(define (valid-design? towel-set design)
  (define (valid d i)
    (define len (string-length d))
    (cond
      [(= len 0) #t]
      [(> i len) #f]
      [else
       (or
        ; current is valid and recur
        (and (set-member? towel-set (substring d 0 i))
             (valid (substring d i) 1))
        ; increase point
        (valid d (add1 i)))])
    )
  (valid design 1))

;; [Setof String] String -> Number
;; returns the count of all possible
;; ways the design can be constructed from the design
(define (permute towel-set design)
  (define (perm d i)
    (define len (string-length d))
    (cond
      [(= len 0) 1]
      [(> i len) 0]
      [else
       (+
        ; check for case where current substring
        ; does match and the the current one not matching
        (if (set-member? towel-set (substring d 0 i))
            (perm (substring d i) 1)
            0)
        ; increase point
        (perm d (add1 i)))])
    )
  (perm design 1))

;; [Setof String] [Listof String] -> Number
;; returns the number of valid designs in the list
(define (count-valid towel-set designs)
  (length (filter ((curry valid-design?) towel-set) designs)))

(define (count-permutations towel-set designs)
  (apply + (map ((curry permute) towel-set) designs)))

(define (run input)
  (define-values (towels designs) (parse-input input))
  (define towel-set (list->set towels))
  (full-solution
   (count-valid towel-set designs)
   (count-permutations towel-set designs))
  )


(module+ test
  (require rackunit)

  (define example-input #<<EOF
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
EOF
    )

  (define-values (towels designs) (parse-input example-input))

  (check-equal? towels
                '("r" "wr" "b" "g" "bwu" "rb" "gb" "br"))

  (check-equal? designs
                '("brwrr"
                  "bggr"
                  "gbbr"
                  "rrbgbr"
                  "ubwu"
                  "bwurrg"
                  "brgr"
                  "bbrgwb"
                  ))
  (define towel-set (list->set towels))
  (check-equal? (valid-design? towel-set "brwrr")
                #t)
  (check-equal? (valid-design? towel-set "bggr")
                #t)
  (check-equal? (valid-design? towel-set "gbbr")
                #t)
  (check-equal? (valid-design? towel-set "rrbgbr")
                #t)
  (check-equal? (valid-design? towel-set "ubwu")
                #f)

  (check-equal? (count-valid towel-set designs)
                6)

  (check-equal? (permute towel-set "brwrr")
                2)
  (check-equal? (permute towel-set "bggr")
                1)
  (check-equal? (permute towel-set "gbbr")
                4)
  (check-equal? (permute towel-set "rrbgbr")
                6)
  (check-equal? (permute towel-set "ubwu")
                0)

  (check-equal? (count-permutations towel-set designs)
                16
                )

  )

