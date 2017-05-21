#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed
(define (dupls-removed sent)
  (define (in-list? elem sent)
    (cond ((empty? sent) #f)
          ((equal? (first sent) elem) #t)
          (else (in-list? elem (bf sent)))))
  (define (dupls-removed-helper sent built-sent)
    (cond ((empty? sent) built-sent)
          ((not (in-list? (first sent) built-sent))
            (dupls-removed-helper (bf sent) (se built-sent (first sent))))
          (else 
            (dupls-removed-helper (bf sent) built-sent))))
  (dupls-removed-helper sent '()))

; Exercise 2 - Define count-word

(define (count-word sent wd)
  (cond ((empty? sent) 0)
        ((equal? (first sent) wd) (+ 1 (count-word (bf sent) wd)))
        (else (count-word (bf sent) wd))))

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
new-if is a procedure so all arguments are evaluated first, since there is a recursive argument
it results in an infinite loop
|#

; Exercise 4 - Define squares

(define (squares sent)
  (if (empty? sent)
    '()
    (se (square (first sent)) (squares (bf sent)))))

; Exercise 5 - Define switch
(define (change-word wd)
    (cond ((or (equal? wd 'I) (equal? wd 'me)) 'you)
          ((equal? wd 'you) 'me)
          (else wd)))
(define (switch sent)
  (define (all-switch sent)
    (if (empty? sent)
      '()
      (se (change-word (first sent)) (all-switch (bf sent)))))
  (if (equal? (first (all-switch sent)) 'me)
    (se 'I (bf (all-switch sent)))
    (all-switch sent)))

; Exercise 6 - Define ordered?

(define (ordered? sent)
  (cond ((empty? (bf sent)) #t)
        ((> (first sent) (item 2 sent)) #f)
        (else (ordered? (bf sent)))))

; Exercise 7 - Define ends-e

(define (ends-e sent)
  (cond ((empty? sent) '())
        ((equal? (last (first sent)) 'e)
          (se (first sent) (ends-e (bf sent))))
        (else
          (ends-e (bf sent)))))

; Exercise 8

#|
Procedures evaluate all arguments first, so if you need to short circuit
in "and" or "or" (such as if one later argument is invalid) then the special form is important
|#
