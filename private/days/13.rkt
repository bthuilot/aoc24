#|

Day 13
https://adventofcode.com/2024/day/13

Had to write out the equation on pen and paper lol

x = a*(x_a) + b*(x_b)
y = a*(y_a) + b*(y_b)

solve for the system of equations
(since we know x, y, x_a, y_a, x_b and y_b)
to get 'a' and 'b' in O(1)

clean

|#

#lang racket/base

(require racket/string)
(require "../utils/point.rkt")
(require "../solution.rkt")

(provide run)

;; Button = (button Number Number)
(struct button (dx dy) #:transparent)

;; Machine = (machine Button Button Point)
(struct machine (a b prize) #:transparent)

(define machine-regex #px"Button A: X\\+(\\d+), Y\\+(\\d+)\\s+Button B: X\\+(\\d+), Y\\+(\\d+)\\s+Prize: X=(\\d+), Y=(\\d+)")

;; String -> [Listof Machine]
(define (parse-machines input)
  (define machines (string-split input "\n\n"))
  (define (parse-machine m)
    (define matches (map string->number (cdar (regexp-match* machine-regex m #:match-select values))))
    (machine (button (list-ref matches 0) (list-ref matches 1))
             (button (list-ref matches 2) (list-ref matches 3))
             (point (list-ref matches 4) (list-ref matches 5))))
  (map parse-machine machines))


;; [Listof Machine] -> Number
;; computes the minimum tokens needed for each
;; machine to get the prize, 0 if you can't get a prize
(define (minimum-tokens machines)
  (define (tokens m)
    (define prize (machine-prize m))
    (define-values (x y) (values (point-x prize) (point-y prize)))
    (define-values (ba bb) (values (machine-a m) (machine-b m)))
    (define-values (xa ya xb yb)
      (values (button-dx ba) (button-dy ba)
              (button-dx bb) (button-dy bb)))
    ;; if it doesnt divide evenly not possible
    (define-values (b remb) (quotient/remainder (- (* x ya) (* y xa)) (- (* ya xb) (* yb xa))))
    (define-values (a rema) (quotient/remainder (- y (* b yb)) ya))
    (if (or (not (= rema 0)) (not (= remb 0)))
        0
        (+ (* a 3) b)))
  (apply + (map tokens machines)))


(define (run input)
  (define machines (parse-machines input))
  (define re-positioned-machines
    (map (λ (m)
           (define prize (machine-prize m))
           (define new-pos 10000000000000)
           (machine (machine-a m) (machine-b m)
                    (point (+ (point-x prize) new-pos) (+ (point-y prize) new-pos))))
         machines))
  
  (full-solution
   (minimum-tokens machines)
   (minimum-tokens re-positioned-machines)))


(module+ test
 (require rackunit)

 (define example-input #<<EOF
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
EOF
)

 (define machines (parse-machines example-input))

 (check-equal? machines
               (list

                (machine (button 94 34) (button 22 67) (point 8400 5400))
                (machine (button 26 66) (button 67 21) (point 12748 12176))
                (machine (button 17 86) (button 84 37) (point 7870 6450))
                (machine (button 69 23) (button 27 71) (point 18641 10279))
                
                )
               )

 (check-equal? (minimum-tokens machines)
               480)

 )
