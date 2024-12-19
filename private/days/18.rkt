#|

Day 18
https://adventofcode.com/2024/day/18

2 things from today

1. actually did dijkstra
2. binary search + dijkstra

feels like sophmore year

|#

#lang racket/base

(provide run)

(require "../utils/point.rkt")
(require "../utils/matrix.rkt")
(require "../solution.rkt")
(require "../utils/priority_queue.rkt")
(require racket/set)
(require racket/string)
(require racket/list)


;; String -> [Listof Point]
;; parses the falling bytes
(define (parse-falling-bytes input)
  (map
   (λ (s) (apply point (map string->number (string-split s ","))))
   (string-split input "\n"))
  )

;; Point
(define start-point (point 0 0))

;; Point
(define end-point (point 70 70))

;; [Listof Point] Point -> Number
;; Finds the shortest path from [start-point]
;; till the given end. Returns -1 if not found
(define (shortest-path falling-bytes end)
  (define max-x (add1 (point-x end)))
  (define max-y (add1 (point-y end)))
  (define corrupt (list->set falling-bytes))
  (define (valid-next? p)
    (and (point-within? p max-x max-y)
         (not (set-member? corrupt p))))
  (define (dijkstra seen q)
    (cond
      [(empty? q) seen]
      [(hash-has-key? seen (peek-pq q))
       (dijkstra seen (drop-pq q))]
      [else
       (define-values (p steps rest-q) (pop-pq q))
       (define next (filter
                     valid-next?
                     (cardinal-points p)))
       (define new-q (foldl
                      (λ (np q-acc) (insert-pq q-acc np (add1 steps)))
                      rest-q
                      next))
       (dijkstra (hash-set seen p steps) new-q)]))

  (define final (dijkstra (hash) (singleton-priority-queue start-point 0)))
  (hash-ref final end (λ () -1)))


;; [Listof Point] Point -> Point
;; binary searches on the index at which
;; the shortest-path fails to find the end
(define (find-cutoff falling-bytes end)
  (define (bin-search low high)
    (define mid (quotient (+ low high) 2))
    (cond
      [(< high low) high] 
      [else
       (define fb (take falling-bytes mid))
       (define res (shortest-path fb end))
       (cond
         [(= res -1) (bin-search low (sub1 mid))]
         [else (bin-search (add1 mid) high)]
         )
       ])
    )
  (define n (bin-search 0 (length falling-bytes)))
  (if (= n -1) -1 (list-ref falling-bytes n)))


(define (run input)
  (define falling-bytes (parse-falling-bytes input))
  (full-solution
   (shortest-path (take falling-bytes 1024) end-point)
   (point->string (find-cutoff falling-bytes end-point))))


(module+ test
 (require rackunit)

 (define example-input #<<EOF
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
EOF
   )

 (define falling-bytes (parse-falling-bytes example-input))

 (check-equal? falling-bytes
               (list
                (point 5 4)
                (point 4 2)
                (point 4 5)
                (point 3 0)
                (point 2 1)
                (point 6 3)
                (point 2 4)
                (point 1 5)
                (point 0 6)
                (point 3 3)
                (point 2 6)
                (point 5 1)
                (point 1 2)
                (point 5 5)
                (point 2 5)
                (point 6 5)
                (point 1 4)
                (point 0 4)
                (point 6 4)
                (point 1 1)
                (point 6 1)
                (point 1 0)
                (point 0 5)
                (point 1 6)
                (point 2 0)))

 

 (check-equal? (shortest-path (take falling-bytes 12) (point 6 6))
               22)

 (check-equal? (find-cutoff falling-bytes (point 6 6))
               (point 6 1))
 )
