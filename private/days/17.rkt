#|

Day 17
https://adventofcode.com/2024/day/17

|#

#lang racket/base


(require racket/list)
(require racket/string)
(require racket/match)
(require "../solution.rkt")
(provide run)

(struct rgstr (a b c) #:transparent)

(struct computer (registers program) #:transparent)

(define computer-regexp #px"Register A: ([0-9]+)\\s+Register B: ([0-9]+)\\s+Register C: ([0-9]+)\\s+Program: ([,0-9]+)")


(define (parse-computer input)
  (define matches (cdar (regexp-match* computer-regexp input #:match-select values)))
  (define regs (map string->number (take matches 3)))
  (define program (map string->number (string-split (last matches) ",")))
  (computer
   (rgstr (car regs) (cadr regs) (caddr regs))
   program))

(define (combo reg op)
  (cond
    [(= op 4) (rgstr-a reg)]
    [(= op 5) (rgstr-b reg)]
    [(= op 6) (rgstr-c reg)]
    [else op]))


(struct write-reg (reg num))
(struct jump (num))
(struct output (num))
;; 'NOP

(define (adv reg operand)
  (define c (combo reg operand))
  (define res (quotient (rgstr-a reg) (expt 2 c)))
  (write-reg 'a res)
  )

(define (bxl reg operand)
  (write-reg 'b (bitwise-xor (rgstr-b reg) operand)))

(define (bst reg operand)
  (define c (combo reg operand))
  (write-reg 'b (modulo c 8)))

(define (jnz reg operand)
  (cond
    [(= (rgstr-a reg) 0) 'NOP]
    [else (jump operand)]
    )
  )

(define (bxc reg operand)
  (write-reg 'b (bitwise-xor (rgstr-b reg) (rgstr-c reg))))
  

(define (out reg operand)
  (define c (combo reg operand))
  (output (modulo c 8)))


(define (bdv reg operand)
  (define c (combo reg operand))
  (define res (quotient (rgstr-a reg) (expt 2 c)))
  (write-reg 'b res)
  )

(define (cdv reg operand)
  (define c (combo reg operand))
  (define res (quotient (rgstr-a reg) (expt 2 c)))
  (write-reg 'c res)
  )


(define (get-instr opcode)
  (match opcode
    [0 adv]
    [1 bxl]
    [2 bst]
    [3 jnz]
    [4 bxc]
    [5 out]
    [6 bdv]
    [7 cdv]))

(define (update-reg reg letter val)
  (rgstr
   (if (equal? letter 'a) val (rgstr-a reg))
   (if (equal? letter 'b) val (rgstr-b reg))
   (if (equal? letter 'c) val (rgstr-c reg))))


(define (run-program comp)
  (define reg (computer-registers comp))
  (define prog (computer-program comp))
  (define (run r p)
    (cond
      [(empty? p) '()]
      [else
       (define opcode (car p))
       (define operand (cadr p))
       (define res ((get-instr opcode) r operand))
       (match res
         ; NOP - do nothing and move along
         ['NOP (run r (cddr p))]
         ; update registers
         [(write-reg l v) (run (update-reg r l v) (cddr p))]
         ; jump to new location
         [(jump ptr) (run r (drop prog ptr))]
         ; techincally this should be (output val)
         ; but it doesnt work?????
         [else (cons (output-num res)  (run r (cddr p)))]
         )
       ]
      )
    )
  (define output (run reg prog))
  (string-join (map number->string output) ",")
  )


(define (run input)
  (define comp (parse-computer input))
  (full-solution
   (run-program comp)
   null 
   ))


(module+ test
 (require rackunit)

 (define example-input #<<EOF
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
EOF
)

 (define comp (parse-computer example-input))

 (check-equal? (computer-registers comp)
               (rgstr 729 0 0))

 (check-equal? (computer-program comp)
               '(0 1 5 4 3 0))

 (check-equal? (run-program comp)
               "4,6,3,5,6,3,5,2,1,0"
               )
 
 )
