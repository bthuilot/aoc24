#|

Day 3
https://adventofcode.com/2024/day/03

Love a regular expression problem.

The "code" could be parsed with a basic regex expression (shown below)
to find the supported instructions. I then parsed then into a set of
types I defined for better readability. Then going throught the matches, i followed
the sematics of the language: if its 'do()', set an accumulator to enabled and vice-versa
if 'dont()'. If its '(mul a b)' and the accumulator is enabled, multiple the numbers and
add it to total.

For part 1 vs part 2 I have a keyword '#:extended?' which when set to false,
ignores the do/don't by always having it enabled

|#

#lang racket/base


(require racket/match)
(require "../solution.rkt")

(provide run)

;; An instruction is one of:
;; - (mul integer integer)
;; - 'enable 
;; - 'disable

;; enabled :: symbol
;; represents the 'do()' instruction
(define enabled 'enable)
;; represents the 'dont()' instruction
(define disabled 'disable)

;; mul :: integer integer
;; struct representing the 'mul(a,b)' instruction
(struct mul (a b) #:transparent)

;; code-regex :: regexp
;; regular expression for paring program
(define code-regex (regexp "mul\\(([0-9]+),([0-9]+)\\)|do(?:n't)?\\(\\)"))

;; parse-instruction :: string -> [instruction]
;; parses the code into a list of instructions
(define (parse-instructions code)
  (map (λ (i)
         (match i
           [(list "do()" _ _) enabled]
           [(list "don't()" _ _) disabled]
           [(list _ a b) (mul (string->number a) (string->number b))]))
       (regexp-match* code-regex code #:match-select values)))

;; run-program :: [instructions] (#:extended? boolean)? -> integer
;; runs the given instructions and returns the results.
;; when #:extended is set to false, "do()" and "don't()" instructions are ignored
(define (run-program instructions #:extended? [extended #t])
  (let ([helper (λ (i acc)
                  (let* ([enable (or (not extended) (cdr acc))] 
                         [result-enabled (if (not (mul? i)) (equal? i enabled) enable)]
                         [result-total (+
                                        (if (and enable (mul? i)) (* (mul-a i) (mul-b i)) 0)
                                        (car acc))])
                    (cons result-total result-enabled)))])
    (car (foldl helper (cons 0 #t) instructions))))
                    

(define (run input)
  (let* ([instructions (parse-instructions input)]
        [part1 (run-program instructions #:extended? #f)]
        [part2 (run-program instructions #:extended? #t)])
    (full-solution part1 part2)))


(module+ test
  (require rackunit)
  ;; Regular
  (define basic-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  (define basic-instr (parse-instructions basic-input))
  (check-equal? basic-instr
                `(,(mul 2 4) ,(mul 5 5) ,(mul 11 8) ,(mul 8 5)))
  (check-equal? (run-program basic-instr #:extended? #f)
                161)
    

  ;; Extended
  (define enabled-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (define enabled-instr (parse-instructions enabled-input))
  (check-equal? enabled-instr
                `(,(mul 2 4) disable ,(mul 5 5) ,(mul 11 8) enable ,(mul 8 5)))
  (check-equal? (run-program enabled-instr #:extended? #t) 48)
  )
