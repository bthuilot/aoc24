#lang racket/base

(require racket/string)

(provide indent)

; Indent : String [Number] -> String
; Indent a string by a given amount
; str: The string to indent
; amt: The amount to indent by
(define (indent str [amt 2])
  (let [(i (make-string amt #\space))]
    (string-append
     i
     (string-replace
      str "\n" (string-append "\n" i)))))