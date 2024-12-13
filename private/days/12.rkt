#|

Day 12
https://adventofcode.com/2024/day/12

|#

#lang racket/base


(require racket/string)
(require racket/function)
(require racket/set)
(require racket/list)
(require "../solution.rkt")
(require "../utils/point.rkt")
(require "../utils/matrix.rkt")

(provide run)

;; Farm = [Matrixof Character]

;; String -> Farm
(define (parse-farm input)
  (map string->list (string-split input)))


(struct measure (area perm) #:transparent)

(define (combine-measure m1 m2)
  (measure (+ (measure-area m1) (measure-area m2))
           (+ (measure-perm m1) (measure-perm m2))))

(define (measure-cost m)
  (* (measure-area m)
     (measure-perm m)))

#;(define (condense-points ps)
  (define (find-home p existing)
    (ormap (λ (adj)
             (or (and (= (point-x p) (point-x adj))
                      (= 1 (abs (- (point-y p) (point-y adj)))))
                 (and (= (point-y p) (point-y adj))
                      (= 1 (abs (- (point-x p) (point-x adj)))))))
           existing))
  (define unique (list->set ps))
  (foldl (λ (p acc)
           (define r (foldl (λ (exisiting a)
                              (define prev (car a))
                              (define found (cdr a))
                              (cond
                                [found (cons (cons exisiting prev) found)]
                                [(find-home p exisiting) (cons (cons (cons p exisiting) prev)
                                                    #t)]
                                [else (cons (cons exisiting prev) found)]))
                            (cons '() #f)
                            acc))
           (define groups (car r))
           (define found (cdr r))
           (if (not found) (cons '(p) acc)
               acc))
             
         '()
         unique))
  

(define (measure-discount-cost m)
  (* (measure-area m)
     (+ 4 (* 2 (- (measure-perm m) 4)))))


;; (struct measure (area perm) #:transparent)

;; (define (combine-measure m1 m2)
;;   (measure (+ (measure-area m1) (measure-area m2))
;;            (+ (measure-perm m1) (measure-perm m2))))

;; (define (measure-cost m)
;;   (* (measure-area m)
;;      (measure-perm m)))

(define (perm-m-f1 outs)
  (length outs))

(define (perm-m-f2 outs)
  (define l (length outs))
  (cond
    [(= l 4) 4]
    [(= l 3) 2]
    [(< l 2) 0]
    [else
     (define p1 (car outs))
     (define p2 (cadr outs))
     (define s (slope-of p1 p2))
     (if (and (= (abs (slope-dx s)) 1)
              (= (abs (slope-dy s)) 1))
         1
         0)]))
(define (fence-cost farm cost-f perm-m-f)
  (define ((same-plot? plot) point)
    (and (in-bounds? farm point)
         (equal? plot (matrix-point farm point))))
  (define (measure-plot fenced point plot)
    (define-values (in out) (partition (same-plot? plot) (cardinal-points point)))

    (define a (foldl (λ (adj a)
                       (define f (car a))
                       (define measures (cdr a))
                       (define adj-plot (matrix-point farm adj))
                       (cond
                         [(set-member? f adj) a]
                         [else
                          (define adj-acc (measure-plot f adj adj-plot))
                          (define adj-measure (cdr adj-acc))
                          (cons (car adj-acc) (combine-measure adj-measure measures))]))
                     (cons (set-add fenced point) (measure 1 (perm-m-f out)))
                     in))
    a)
  
  (struct acc (fenced cost))
  (define (fold point plot a)
    (define fenced (acc-fenced a))
    (define cost (acc-cost a))
    (cond
      [(set-member? fenced point) a]
      [else
       (define a (measure-plot fenced point plot))
       (define plots (car a))
       (define measures (cdr a))
       (print plot) (print " ")(print point)
       (displayln measures)
       (acc plots (+ cost (cost-f measures)))]))
  (define final (matrix-fold-point fold (acc (set) 0) farm))
  (acc-cost final))

(define (run input)
  (define farm (parse-farm input))
  (full-solution
   (fence-cost farm measure-cost perm-m-f1)
   (fence-cost farm measure-discount-cost perm-m-f2)
   )
  
  )


(module+ test
  (require rackunit)

  (define example-1 #<<EOF
AAAA
BBCD
BBCC
EEEC
EOF
    )

  (define example-2 #<<EOF
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
EOF
    )

  (define example-3 #<<EOF
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
EOF
    )

  (define farm-1 (parse-farm example-1))
  (define farm-2 (parse-farm example-2))
  (define farm-3 (parse-farm example-3))

  (check-equal? farm-1
                '((#\A #\A #\A #\A)
                  (#\B #\B #\C #\D)
                  (#\B #\B #\C #\C)
                  (#\E #\E #\E #\C)))

  (check-equal? farm-2
                '((#\O #\O #\O #\O #\O)
                  (#\O #\X #\O #\X #\O)
                  (#\O #\O #\O #\O #\O)
                  (#\O #\X #\O #\X #\O)
                  (#\O #\O #\O #\O #\O)))

  (check-equal? (fence-cost farm-1 measure-cost perm-m-f1)
                140)

  (check-equal? (fence-cost farm-2 measure-cost perm-m-f1)
                772)
  (check-equal? (fence-cost farm-3 measure-cost perm-m-f1)
                1930)
  
  (check-equal? (fence-cost farm-1 measure-discount-cost perm-m-f2)
                80)

  (check-equal? (fence-cost farm-2 measure-discount-cost perm-m-f2)
                436)

  (check-equal? (fence-cost farm-3 measure-discount-cost perm-m-f2)
                1206)
  

  )
