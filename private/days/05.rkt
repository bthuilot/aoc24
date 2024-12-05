#|

Day 5
https://adventofcode.com/2024/day/05

Hash maps are the best to work with in a functional
langauge but I made do lol.

First I parsed all the rules into a hash table mapping
page number to a set of all page numbers that can't appear
before it.

I then use this table and go through each list of page numbers.
I start the iteration with an empty set and at each index I add
the current page number to the 'seen'. I additionally look up the
current page number in the table and check if the intersection
of the set of 'not allowed before' and 'seen' is not empty. If it
is, I know that one came before and the list is incorrect.

In part 2, I just extend the previous one a bit where when an incorrect
list is found, I traverse the list in reverse from the current point,
and remove the elements I see from the intersect results. Once that result
is empty (i.e. all not-allowed have been passed) I can place the page number
that was incorrect back in

|#

#lang racket/base

(require racket/set)
(require racket/string)
(require racket/list)
(require "../solution.rkt")

(provide run)

;; Rule-Map is a [Hash-of string] -> [Set-of string]
;; The key represents a page and the value is
;; a set of pages that must not come before it

;; parse-rules :: string -> Rule-Map
;; parses the input rules into a Rule-Map
(define (parse-rules rules-def)
  (let* ([rules (map (λ (r) (string-split r "|")) (string-split rules-def "\n"))]
         [h (make-hash)]
         [get-rules (λ (r) (hash-ref h r set))])
    (map (λ (rule)
           (hash-set*! h (car rule)
                       (set-add (get-rules (car rule)) (cadr rule))))
         rules)
    h
    ))

;; a Page is a [List-of string]

;; parse-pages :: string -> [Page]
;; parses each line into a page
(define (parse-pages pages-def)
  (map
   (λ (p) (string-split p ","))
   (string-split pages-def "\n")))


;; valid-page? :: Rule-Map Page -> boolean
;; 
(define (valid-page? rules page)
  (letrec ([valid? (λ (remain seen)
                     (or (empty? remain)
                         (and (set-empty? (set-intersect (hash-ref rules (car remain) set) seen))
                              (valid? (cdr remain) (set-add seen (car remain))))))])
    (valid? page (set))))
 
;; get-median :: [List-of string] -> integer
;; returns the median of the list
;; and converts to a number
(define (get-median l)
  (string->number (list-ref l (quotient (length l) 2))))

;; fix-incorrect :: Rule-Map Page -> Page
;; fixes an incorrect page so that it is correct
(define (fix-incorrect rules incorrect)
  (letrec ([intersect (λ (p seen) (set-intersect (hash-ref rules p (λ () (set))) seen))]
           [insert (λ (p before l)
                     (if (or (set-empty? before) (empty? l)) (cons p l)
                         (cons (car l)
                               (insert p (set-remove before (car l)) (cdr l)))))]
           [fix (λ (fixed remain seen)
                  (if (empty? remain) (reverse fixed)
                      (fix (insert  (car remain) (intersect (car remain) seen) fixed)
                           (cdr remain)
                           (set-add seen (car remain)))
                      ))])
    (fix '() incorrect (set))))

(define (run input)
  (let*-values ([(split-input) (string-split input "\n\n")]
                ;; parse both rules and pages
                [(rules pages) (values (parse-rules (car split-input)) (parse-pages (cadr split-input)))]
                ;; partition list into correct and incorrect
                [(correct incorrect) (partition (λ (p) (valid-page? rules p)) pages)]
                ;; part1 is median of correct
                [(part1) (apply + (map get-median correct))]
                ;; part2 is median of incorrect that have been fixed
                [(part2) (apply + (map (λ (p) (get-median (fix-incorrect rules p))) incorrect))])
    (full-solution part1 part2)))


(module+ test
  (require rackunit)

  (define example-input #<<EOF
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
EOF
    )

  (define example-input-split (string-split example-input "\n\n"))
  (define rules (parse-rules (car example-input-split)))
  (define pages (parse-pages (cadr example-input-split)))

  (check-equal? (make-hash `(("47" . ,(list->set '("53" "13" "29" "61")))
                           ("97" . ,(list->set '("13" "61" "47" "29" "53" "75")))
                           ("75" . ,(list->set '("29" "53" "47" "61" "13")))
                           ("61" . ,(list->set '("13" "29" "53")))
                           ("29" . ,(list->set '("13")))
                           ("53" . ,(list->set '("29" "13")))
                           ))
                rules
                )
  (check-equal? (valid-page? rules '("75" "47" "61" "53" "29"))
                #t)
  (define-values (correct incorrect) (partition (λ (p) (valid-page?  rules p)) pages))

  (check-equal? (apply + (map get-median correct))
                143)

  (check-equal? (fix-incorrect rules '("75" "97" "47" "61" "53"))
                '("97" "75" "47" "61" "53"))
  (check-equal? (fix-incorrect rules '("61" "13" "29"))
                '("61" "29" "13"))
  (check-equal? (fix-incorrect rules '("97" "13" "75" "29" "47"))
                '("97" "75" "47" "29" "13"))
  (check-equal? (apply + (map (λ (p) (get-median (fix-incorrect rules p))) incorrect))
                123)
  )
