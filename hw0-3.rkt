#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  (cond ((= (quotient secs 60) 0)
          (sentence secs 'seconds))
        ((< (quotient secs 60) 60)
          (sentence (quotient secs 60) 'minutes (describe-time (remainder secs 60))))
        ((< (quotient secs 3600) 24)
          (sentence (quotient secs 3600) 'hours (describe-time (remainder secs 3600))))
        (else
          (sentence (quotient secs 86400) 'days (describe-time (remainder secs 86400))))))
; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (cond ((empty? sent) '())
        ((not (equal? (first sent) wd))
          (se (first sent) (remove-once wd (bf sent))))
        (else
          (remove-once '() (bf sent)))))

; Exercise 3 - Define differences
(define (differences nums)
  (if (or (empty? nums) (empty? (bf nums)))
    (se '())
    (se (- (item 2 nums) (first nums)) (differences (bf nums)))))

; Exercise 4 - Define location
(define (location small big)
  (define (loc-helper small big n)
    (cond ((empty? big) #f)
          ((equal? small (first big)) n)
          (else (loc-helper small (bf big) (+ n 1)))))
  (loc-helper small big 1))

; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent)
    '()
    (se (first (first sent)) (initials (bf sent)))))

; Exercise 6 - Define copies
(define (copies num wd)
  (if (= num 0)
    '()
    (se wd (copies (- num 1) wd))))

; Exercise 7 - Define gpa
(define (base-grade grade)
    (let ((letter (first grade))) 
      (cond ((equal? letter 'A) 4)
            ((equal? letter 'B) 3)
            ((equal? letter 'C) 2)
            ((equal? letter 'D) 1)
            (else 0))))
(define (grade-modifier grade)
  (let ((modif (bf grade)))
    (cond ((empty? modif) 0)
          ((equal? modif '+) .33)
          ((equal? modif '-) -.33))))
(define (total-gpa grades)
    (if (empty? grades)
        0
        (let ((grade (first grades)))
          (+ (base-grade grade) (grade-modifier grade) (total-gpa (bf grades))))))
(define (gpa grades)
  (if (= (total-gpa grades) 0)
    0
    (/ (total-gpa grades) (length grades))))

; Exercise 8 - Define repeat-words
(define (repeat num wd)
  (if (= num 0)
    '()
    (se wd (copies (- num 1) wd))))
(define (repeat-words sent)
  (cond ((empty? sent) '())
        ((number? (first sent))
          (se (repeat (first sent) (item 2 sent)) (repeat-words (bf (bf sent)))))
        (else
          (se (first sent) (repeat-words (bf sent))))))

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  (cond ((and (empty? sent1) (empty? sent2))
          #t)
        ((or (and (empty? sent1) (not (empty? sent2))) (and (not (empty? sent1)) (empty? sent2)))
          #f)
        (else
          (if (= (count (first sent1)) (count (first sent2)))
            (same-shape? (bf sent1) (bf sent2))
            #f))))
