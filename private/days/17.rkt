#|

Day 17
https://adventofcode.com/2024/day/17

Part 1, ez. Love passing around functions

Part 2, brute force. Im not gonna reverse engineer
the whole instruction set, so instead just built
in a "check" to the original function and fail fast if
we encouter a number thats not expected. Additionally
only search octal numbers, since it will be modulo 8.

|#

#lang racket/base


(require racket/list)
(require racket/string)
(require racket/match)
(require "../solution.rkt")
(provide run)

;; Register = (rgstr Number Number Number)

;; (rgstr Number Number Number)
;; represents the regsiters of the computer
(struct rgstr (a b c) #:transparent)

;; Computer = (computer Register [Listof Number])

;; (computer Register [Listof Number])
;; represents a computer's registers and the
;; program to run
(struct computer (registers program) #:transparent)

(define computer-regexp #px"Register A: ([0-9]+)\\s+Register B: ([0-9]+)\\s+Register C: ([0-9]+)\\s+Program: ([,0-9]+)")

;; String -> Computer
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

;; RegisterID is one of
;; - 'a
;; - 'b
;; - 'c

;; An InstructionResult is one of
;; - (write-reg RegisterID Number)
;; - (jump Number)
;; - (output Number)
;; - 'NOP

;; (write-reg RegisterID Number)
;; represents a result to write a number
;; to the given regsiter
(struct write-reg (reg num))

;; (jump Number)
;; represents changing the instruction
;; pointer the number given
(struct jump (num))

;; (output Number)
;; represents writing a new output number
(struct output (num))


;; Instruction = [Register Number -> InstructionResult]


;; Instructions ;;

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
    [else (jump operand)]))

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
  (write-reg 'c res))


;; Number -> Instruction
;; returns the Instruction for the given opcode
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

;; Regsiter RegistryID Number -> Register
(define (update-reg reg letter val)
  (rgstr
   (if (equal? letter 'a) val (rgstr-a reg))
   (if (equal? letter 'b) val (rgstr-b reg))
   (if (equal? letter 'c) val (rgstr-c reg))))

;; [Listof Number] Number [void -> [Listof Number]]
;; checks the next out is expected and if so recurs
;; otherwise, returns #f
(define (check-expected e out recur)
  (cond
    [(empty? e) #f]
    [(not (equal? (car e) out)) #f]
    [else
     (define r (recur))
     (if (not r) #f
         (cons out r))]))

;; Computer ([Listof Number] | #f)? -> [Listof Number] | #f
;; runs the program and returns the result.
;; If expected is set, checks each output matches the next
;; expected one. If not returns #f
(define (run-program comp [expected #f])
  (define reg (computer-registers comp))
  (define prog (computer-program comp))
  (define (run r p e)
    (cond
      [(empty? p)
       (if (and e (not (empty? e))) #f '())]
      [else
       (define opcode (car p))
       (define operand (cadr p))
       (define res ((get-instr opcode) r operand))
       (match res
         ; NOP - do nothing and move along
         ['NOP (run r (cddr p) e)]
         ; update registers
         [(write-reg l v) (run (update-reg r l v) (cddr p) e)]
         ; jump to new location
         [(jump ptr) (run r (drop prog ptr) e)]
         ; techincally this should be (output val)
         ; but it doesnt work?????
         [else
          (if (not e) (cons (output-num res) (run r (cddr p) e))
              (check-expected e (output-num res) (λ () (run r (cddr p) (cdr e)))))]
         )]))
  
  (define output (run reg prog expected))
  (if (not output) #f
      (string-join (map number->string output) ",")))

;; Number -> Number
;; increments a number as an octal
(define (increment-octal octal-int)
  (let* ([decimal (string->number (number->string octal-int) 8)]
         [incremented (+ decimal 1)]
         [new-octal (string->number (number->string incremented 8) 10)])
    new-octal))

;; Computer -> Number
;; quine returns the value of register
;; a that needs to be set in order for the prog
;; to output itself
(define (quine comp)
  (define reg (computer-registers comp))
  (define prog (computer-program comp))
  (define (test a)
    (define new-reg (update-reg reg 'a a))
    (define new-comp (computer new-reg prog))
    (if (run-program new-comp prog) a
        (test (increment-octal a))))
  (test 0))


(define (run input)
  (define comp (parse-computer input))
  (full-solution
   (run-program comp)
   (quine comp)
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

 (define example-input-2 #<<EOF
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
EOF
)

 (define comp-2 (parse-computer example-input-2))

 (check-equal? (quine comp-2)
               117440)
 )
