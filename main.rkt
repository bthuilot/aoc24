#lang racket/base

(require racket/format)
(require racket/file)
(require racket/match)
(require racket/exn)

(module+ test
  (require rackunit))


;; This is the header that will be displayed when the program is run
(define header #<<EOF
##################
# Advent of Code #
# ---- 2024 ---- #
##################

EOF
  )

; Day -> Pair String String
(define (run-day day)
  (let* ([formatted-number (~r day #:min-width 2 #:pad-string "0")]
         [module-path (format "days/~a.rkt" formatted-number)]
         [input-path (format "inputs/~a.txt" formatted-number)])

    (unless (file-exists? module-path)
      (error (format "day ~a not implemented" day)))
    ;; read from input file
    (define input (file->string input-path))
    
    (define run (dynamic-require module-path 'run (λ () (error 'main "unable to load day ~a, please implement run function" day))))
    (run input)))



(module+ main
  (require racket/cmdline)
  (require racket/string)
  (define selected-days (box '()))
  (command-line
    #:program "aoc-2024"
    #:once-each
    [("-d" "--days") days "A whitelist of AOC day challenges to run"
                     (set-box! selected-days (map string->number (string-split days ",")))]
    #:args ()
    (define days (let ([days (unbox selected-days)])
                   (if (null? days)
                       (begin
                         (printf "No days selected, running all days~n")
                         (build-list 25 add1))
                       days)))
    ;; Check to make sure the days are valid
    ;; 0 is a valid day, but it is not a valid day for AOC
    (or (not (member (λ (x) (and (integer? x) (<= 0 x) (<= x 25))) days))
        (error "invalid days given, must be a list of integers between 1 and 25 inclusive"))

    (displayln header)
    
    (for [(day days)]
      ;; Run the day
      (printf "= Running day ~a =~n" day)
      (with-handlers ([exn:fail? (λ (e) (displayln (string-append "  " (exn-message e))) (void))])
        (let ([results (run-day day)])
          (match results
            [(cons (? string?) (? string?)) (printf "  part1: ~a~n  part2: ~a~n"
                                                    (car results)
                                                    (cdr results))]
            [(cons (? string?) _) (printf "  part1: ~a~n  part2: not implemented~n" (car results))]
            [(cons _ (? string?)) (printf "  part1: not implemented~n  part2: ~a~n" (cdr results))]
            [_ (printf "  part1: not implemented~n  part2: not implemented~n")])))
      (displayln ""))))


;; (module+ test
;;   (check-equal? (+ 2 2) 4))
