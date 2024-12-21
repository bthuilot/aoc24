#lang racket/base

(provide
 
 remove-pq

 insert-pq

 pop-pq

 drop-pq

 priority-queue

 peek-pq

 singleton-priority-queue
 )

(require racket/list)



(struct pq (i w))

(define (priority-queue)
  '())

(define (singleton-priority-queue i w)
  `(,(pq i w)))

(define (remove-pq q i)
  (cond
    [(empty? q) q]
    [(equal? (pq-i (car q)) i) (cdr q)]
    [else (cons (car q) (remove-pq (cdr q) i))]))

(define (insert-pq q i w)
  (cond
    [(empty? q) (cons (pq i w) q)]
    [(or (> (pq-w (car q)) w)
         (= (pq-w  (car q)) -1)) (cons (pq i w) (remove-pq q i))]
    [(and (equal? (pq-i (car q)) i) (< (pq-w (car q)) w)) q]
    [else (cons (car q) (insert-pq (cdr q) i w))]))

(define (pop-pq q)
  (values (pq-i (car q)) (pq-w (car q)) (cdr q)))

(define (drop-pq q)
  (cdr q))

(define (peek-pq q)
  (pq-i (car q)))

(module+ test
  (require rackunit)

  ;; (check-equal? (insert-pq '((t . -1) (e . -1) (s . -1)) 's 0)
 ;;               '((s . 0) (t . -1) (e . -1))
 ;;               )
 ;; (check-equal? (insert-pq '((t . 0) (e . -1) (s . -1)) 's 20)
 ;;               '((t . 0) (s . 20) (e . -1))
 ;;               )
 ;; (check-equal? (insert-pq '((t . 1) (e . 5) (s . 7)) 'e 10)
 ;;               '((t . 1) (e . 5) (s . 7))
 ;;               )

 ;; (check-equal? (insert-pq '((t . 1) (e . 5) (s . 7)) 'e 4)
 ;;               '((t . 1) (e . 4) (s . 7))
 ;;               )
 ;; (check-equal? (insert-pq '((t . 2) (e . 5) (s . 7)) 'e 1)
 ;;               '((e . 1) (t . 2) (s . 7))
 ;;               )

 ;; (check-equal? (insert-pq '((t . 1) (e . 5) (s . 7)) 'e 6)
 ;;               '((t . 1) (e . 5) (s . 7))
 ;;               )

;;  (let-values ([(p q pq) (pop-pq '((t . 1) (e . 5) (s . 7)))]
;;               )
     
;;      (check-equal? p
;;                    't
;;      )

;;      (check-equal? q
;;                    1
;;                    )
;;      (check-equal? pq
;;                    '((e . 5) (s . 7))
;;       )
     

;; )
  )