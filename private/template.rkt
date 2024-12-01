#lang racket/base

(require racket/format)
(require "./display.rkt")

(provide create-day-file)


(define day-template #<<EOF
#|

Day ~a
https://adventofcode.com/2024/day/~a

|#

#lang racket/base

(require "../solution.rkt")

(define (run input)
  (let ([part1 null]
        [part2 null])
    ())


;; (module+ test
;;  (require rackunit))

EOF
  )

(define (create-day-file num)
  (let* ([formatted-num (pad num #\0 2)]
         [module-path (format "private/days/~a.rkt" formatted-num)]
         [template   (format day-template num formatted-num)])
    (if (file-exists? module-path)
        (error "not generating new day, file already exists")
        (void))
    (with-output-to-file module-path (λ () (display template)))))