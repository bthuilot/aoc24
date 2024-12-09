#|

Day 9
https://adventofcode.com/2024/day/09

This day was a bit of a mess.

I'm just going to commit what I have for now
and maybe one day come back and clean this all
up and add proper comments.

First part I calculate as I traverse the list,
getting checksum of each file I come across, or
if its empty, pulling files from the back of the list.

The second part I parse the disk into a 'Disk'
so i dont need to worry about remembering indexes.
Then I attempt to condense, starting from files in the back
and attempting to replace empty spaces (that are big enough)
with the file as I move it forward.

All of these functions are hard to keep the logic and variables
in my head, so a refactor is definitely needed for it to be readable.

|#

#lang racket/base

(require racket/list)
(require racket/set)
(require "../solution.rkt")

(provide run)

;; FreeSpace = (free-space Number)
;; represents a block of free space on disk
(struct free-space (amt) #:transparent)

;; File = (file-space Number Number)
;; represents a file on the disk
(struct file-space (id amt) #:transparent)

;; DiskMap = [Listof Number]

;; DiskEntry = FreeSpace | File

;; Disk = [Listof DiskEntry]
;; A disk is a list of numbers of #f.
;; A number represents a file on the hard disk
;; and #f represents free space


(define (char->number c)
  (- (char->integer c) 48))

(define (parse-disk-map input)
  (map char->number (string->list input)))
  

;; DiskMap -> Disk
;; parses a disk map into the actual contents of the disk
(define (parse-disk disk-map)
  (define (parse-entry num acc)
    (define i (length acc))
    (cons
     (if (even? i) (file-space (quotient i 2) num) (free-space num))
     acc))
  (reverse (foldl parse-entry '() disk-map)))

;; Disk -> Number
;; Performs a disk checksum
(define (disk-checksum disk)
  (cdr (foldl (λ (entry acc)
                (define index (car acc))
                (define total (cdr acc))
                (cond
                  [(free-space? entry) (cons (+ index (free-space-amt entry)) total)]
                  [else
                   (define amt (file-space-amt entry))
                   (define id (file-space-id entry))
                   (define new-total (apply + total (build-list amt (λ (i) (* (+ i index) id)))))
                   (cons (+ index amt) new-total)]
                  ))
              (cons 0 0)
              disk)))

;; DiskMap -> Number
(define (part1 disk-map)
  ;; This calculates the check sum for a disk
  ;; should be used with 'build-list' where it
  ;; has a starting index and a disk id, then called
  ;; with 0,1,2, etc for the the amount of files in a given section
  ;; of disk with same id
  (define ((accc disk-id start-index) i)
    (define index (+ i start-index))
    (* index disk-id))
  ;; this starts from the back and moves 'amt' of files on the disk
  ;; to the 'front' by adding to total. It also returns the 'back' updated
  ;; with the removed disk
  (define (take-from-back back amt back-index total)
    (cond
      [(or (empty? back) (<= amt 0)) (values total back)]
      [(odd? back-index) (take-from-back (cdr back) amt (sub1 back-index) total)]
      [else
       (define file-amt (car back))
       (define new-total (append total (build-list (min amt file-amt) (accc (quotient back-index 2) (length total)))))
       (cond
         [(= amt file-amt) (values new-total (cdr back))]
         [(< amt file-amt) (values new-total (cons (- file-amt amt) (cdr back)))]
         [else (take-from-back (cdr back) (- amt file-amt) (sub1 back-index) new-total)])]))
  ;; Goes through the disk map calculating total
  ;; when it finds a empty space, calls 'take-from-back' to fill
  ;; the space and add to total
  (define (parse dm dm-index total)
    (define disk-id (quotient dm-index 2))
    (cond
      [(empty? dm) total]
      [(even? dm-index)
       ;; case where its a file so we sould update total
       (parse (cdr dm)
              (add1 dm-index)
              (append total (build-list (car dm) (accc disk-id (length total)))))]
      [else
       ;; empty space, we should take some files from back
       (define free-amt (car dm))
       (define next-dm-index (+ dm-index 1))
       (define back-dm-index (+ (length dm) dm-index -1))
       (define-values (new-total back) (take-from-back (reverse (cdr dm)) free-amt back-dm-index total))
       (parse (reverse back) next-dm-index new-total)]))
  (apply + (parse disk-map 0 '())))


(define (part2 disk)
  ;; Disk File -> Disk
  ;; finds and fills a space in disk 'd'
  ;; with the file f, or appends to end if
  ;; no spaces are found
  (define (find-space d f)
    (cond
      [(empty? d) (list f)]
      [(and (free-space? (car d))
            (>= (free-space-amt (car d))
                (file-space-amt f)))
       (define diff (- (free-space-amt (car d))
                       (file-space-amt f)))
       (if (= diff 0)
           (cons f (cdr d))
           ;; (cons f (cons (free-space diff) d))
           (cons f (cons (free-space diff) (cdr d)))
           )]
      [else (cons (car d) (find-space (cdr d) f))]))
  ;; Condense starts from a reversed disk 'todo'
  ;; and when it finds a file, attempts to fill
  ;; an empty space with the file. Records which IDs
  ;; its seen so it only processes each once
  (define (condense todo done completed)
    (cond
      [(empty? todo) done]
      [(free-space? (car todo)) (condense (cdr todo) 
                                          (cons (car todo) done)
                                          completed)]
                                
                                
      [(set-member? completed (file-space-id (car todo))) (condense
                                                           (cdr todo)
                                                           (cons (car todo) done)
                                                           completed)]
      [else
       (define f (car todo))
       (define f-amt (file-space-amt f))
       (define f-id (file-space-id f))
       (condense 
        (reverse (find-space (reverse (cons (free-space f-amt) (cdr todo))) (car todo)))
        done
        (set-add completed f-id))]))
  ;; codense then perform checksum
  (disk-checksum (condense (reverse disk) '() (set))))
        

(define (run input)
  (define disk-map (parse-disk-map input))
  (define disk (parse-disk disk-map))
  
  (full-solution
   (part1 disk-map)
   (part2 disk)
   ))

(module+ test
  (require rackunit)

  (check-equal? (char->number #\0) 0)
  (check-equal? (char->number #\5) 5)
  (check-equal? (char->number #\9) 9)

  (define example-input "2333133121414131402")

  (define disk-map (parse-disk-map example-input))

  (check-equal? (part1 disk-map)
                1928)

  (define disk (parse-disk disk-map))
  (check-equal? disk
                `(,(file-space 0 2)
                  ,(free-space 3)
                  ,(file-space 1 3)
                  ,(free-space 3)
                  ,(file-space 2 1)
                  ,(free-space 3)
                  ,(file-space 3 3)
                  ,(free-space 1)
                  ,(file-space 4 2)
                  ,(free-space 1)
                  ,(file-space 5 4)
                  ,(free-space 1)
                  ,(file-space 6 4)
                  ,(free-space 1)
                  ,(file-space 7 3)
                  ,(free-space 1)
                  ,(file-space 8 4)
                  ,(free-space 0)
                  ,(file-space 9 2)
                  ))

  (check-equal? (part2 disk)
                2858)

  )
