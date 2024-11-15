#lang racket/base

(require racket/string)

(provide indent pad)

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

;; pad : String Char Number -> String
;; Pad a string with a given character to a given length
;; str: The string to pad
;; chr: The character to pad with
;; len: The length to pad to
;; back?: Whether to pad at the back or front (default front)
(define (pad str chr len [back? #f])
  (let [(i (- len (string-length str)))]
    (cond
      [(<= i 0) str]
      [back? (string-append str (make-string i chr))]
      [else (string-append (make-string i chr) str)])))
  