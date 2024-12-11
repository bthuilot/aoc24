#lang racket/base

(provide
 ;; Character -> Number
 char->number
 )

(require racket/string)

;; Character -> Number
;; Parses a character into
;; a Number
(define (char->number c)
  (- (char->integer c) 48))