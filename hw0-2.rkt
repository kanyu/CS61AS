#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10
;2. Compound Expression (3 Atoms)
(+ 5 5)
;3. Compound Expression (4 Atoms)
(+ 1 1 8)
;4. Compound Expression (1 Atom and 2 subexpressions)
(* (+ 1 4) (+ 1 1))
;5. Any Other Kind Expression
(+ 1 1 1 1 1 1 1 1 1 1)

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  (word (first wd) (first (bf wd))))

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y)))

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word (first (first sent)) (first (last sent))))

;Exercise 2 - Define teen?
(define (teen? num)
  (if (and (>= num 13) (<= num 19))
    #t
    #f))

;Exercise 3 - Define indef-article
(define (vowel? c)
  (if (or (equal? c 'a)
          (equal? c 'e)
          (equal? c 'i)
          (equal? c 'o)
          (equal? c 'u))
    #t
    #f))
(define (indef-article wd)
  (if (vowel? (first wd))
    (sentence 'an wd)
    (sentence 'a wd))
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (sentence (bl sent) 'and (last sent)))

;Exercise 5 - Define query
(define (query sent)
  (sentence (item 2 sent)
            (first sent)
            (bl (bf (bf sent)))
            (word (last sent) '?)))

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (cond ((and (equal? (last time) 'am) (equal? (first time) 12)) 0)
        ((and (equal? (last time) 'pm) (equal? (first time) 12)) 12)
        ((equal? (last time) 'am) (first time))
        (else (+ (first time) 12))))

(define (american-time time)
  (cond ((equal? time 12) (sentence time 'pm))
        ((equal? time 0) (sentence 12 'am))
        ((<= time 12) (sentence time 'am))
        (else (sentence (- time 12) 'pm))))

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (cond ((< secs 60) (sentence secs 'seconds))
        ((< secs 3600) (sentence (/ secs 60) 'minutes))
        ((< secs 86400) (sentence (/ secs 3600) 'hours))
        (else (sentence (/ secs 86400) 'days))))

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective worde)
  (se (word adjective 'est) worde))

#|
Can't have redefinition of "word"
|#