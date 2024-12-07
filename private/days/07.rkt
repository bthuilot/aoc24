#|

Day 7
https://adventofcode.com/2024/day/07

Super easy day for a functional language.

Everything was just multiple recursion using
different 'operators', with the recursion results
all or'd together. Part 1 vs part 2 was just including
the 'concat-nums' function in the operators.

|#

#lang racket/base

(require racket/string)
(require racket/list)
(require "../solution.rkt")

(provide run)

;; Equation = (equation Number [Listof Number])

;; (equation Number [Listof Number])
(struct equation (test nums) #:transparent)

;; Operator = (Number Number -> Number)

;; String -> [Listof Equation]
;; parses the input into a list of equations
(define (parse-equations input)
  (define lines (string-split input "\n"))
  (define (parse-line l)
    (define parts (string-split l ": "))
    (equation (string->number (car parts))
              (map string->number (string-split (cadr parts)))))
  (map parse-line lines))

;; Number Number -> Number
;; concats 2 numbers together
(define (concat-nums a b)
  ;; at first i tried to do this math based thing with log,
  ;; however that didnt work out and this is easier
  (string->number
   (string-append (number->string a) (number->string b))))


;; [Listof Operator] -> (Equation -> Boolean)
;; determines if the equation is valid using
;; the operators in the given list
(define ((valid-equation? operators) equation)
  (define test (equation-test equation))
  (define nums (equation-nums equation))
  (define (valid? total ns)
    (define (recur op)
      (valid? (op total (car ns)) (cdr ns)))
    (or
     ;; Valid if 
     ;; total == test and list is empty or
     (and (= total test) (empty? ns))
     ;; or
     ;; its not empty and recuring using on the operators
     ;; results in the complete equation
     (and (not (empty? ns))
          (ormap recur operators))))
  (valid? (car nums) (cdr nums)))

;; [Listof Equation] -> Number
;; returns the calibration result from
;; a list of equations
(define (calibration-result eqs)
  (apply + (map equation-test eqs)))

;; [Listof Operator]
;; operators valid for part 1
(define part1-operators `(,+ ,*))

;; [Listof Operator]
;; operators valid for part 2
(define part2-operators `(,+ ,* ,concat-nums))


(define (run input)
  (define eqs (parse-equations input))

  (full-solution
   (calibration-result (filter (valid-equation? part1-operators) eqs))
   (calibration-result (filter (valid-equation? part2-operators) eqs))))
  
(module+ test
  (require rackunit)

  (define example-input #<<EOF
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
EOF
    )

    (define example-equations (parse-equations example-input))
    (check-equal? example-equations
                  (list (equation 190 '(10 19))
                        (equation 3267 '(81 40 27))
                        (equation 83 '(17 5))
                        (equation 156 '(15 6))
                        (equation 7290 '(6 8 6 15))
                        (equation 161011 '(16 10 13))
                        (equation 192 '(17 8 14))
                        (equation 21037 '(9 7 18 13))
                        (equation 292 '(11 6 16 20 ))))

    (define valid (filter (valid-equation? part1-operators) example-equations))
    (check-equal? (calibration-result valid)
                  3749)

    (check-equal? (concat-nums 100 20)
                  10020)
    (check-equal? (concat-nums 20 1)
                  201)
    (check-equal? (concat-nums 1 456)
                  1456)
    (check-equal? (concat-nums 8 6)
                  86)


    (define valid-concat (filter (valid-equation? part2-operators) example-equations))
    (check-equal? (calibration-result valid-concat)
                  11387)
  )
