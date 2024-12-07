#lang racket/base

; Racket imports
(require racket/format)
(require racket/file)
(require racket/match)
(require racket/exn)
(require racket/string)
(require racket/cmdline)

; Relative imports
(require "private/solution.rkt")
(require "private/template.rkt")
(require "private/utils/display.rkt")


;; String
;; This is the header that will be displayed when the program is run
(define header #<<EOF
##################
# Advent of Code #
# ---- 2024 ---- #
##################

EOF
  )

;; Exn -> Void
;; Prints the error message of an exception
(define (print-err e)
  (define msg (exn-message e))
  (displayln (string-append "ERROR: " msg)))

;; [Listof Day]) -> Void
;; Runs the main program for the advent of code
(define (main days)
  (displayln header)
  (for [(day days)]
    (printf "= Running day ~a =~n" day)
    (with-handlers
      ([exn:fail? print-err])
      (define solution (solution->string (run-day day)))
      (displayln (indent solution)))))


;; Day -> Solution
;; Runs the solution for a given day by importing the module
;; and running the run function
(define (run-day day)
  (let* ([formatted-number (~r day #:min-width 2 #:pad-string "0")]
         [module-path (format "private/days/~a.rkt" formatted-number)]
         [input-path (format "inputs/~a.txt" formatted-number)])
    ;; Check if the module exists
    (unless (file-exists? module-path)
      (error (format "day ~a not implemented" day)))
    ;; Check if the input exists
    (unless (file-exists? input-path)
      (error (format "day ~a input not found" day)))
    ;; import the module and run the function
    (define run
      (dynamic-require module-path
                       'run
                       (λ () (error 'main "unable to load day ~a, please implement run function" day)))
      )
    ;; Run the function with the input
    (run (file->string input-path))))



(module+ main
  (define selected-days (box (build-list 25 add1)))
  (define generate-day (box null))
  (command-line
    #:program "aoc-2024"
    #:once-any
    [("-g" "--generate") day "Generate a new day from the template"
                       (set-box! generate-day day)]
    [("-d" "--days") days "A whitelist of AOC day challenges to run"
                     (set-box! selected-days (map string->number (string-split days ",")))]
    #:args ()
    (if (not (null? (unbox generate-day)))
        (begin
          (create-day-file (unbox generate-day))
          (exit))
        (void))
    (let [(days (unbox selected-days))]
      (or (not (member (λ (x) (and (integer? x) (<= 0 x) (<= x 25))) days))
          ;; 0 is a test day, so it is considered valid
          ;; but the error message doesn't include it since its not valid for AOC
          error "invalid days given, must be a list of integers between 1 and 25 inclusive")
      (main days))))

